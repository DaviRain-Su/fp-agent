open! Base

type status = Completed | Failed | Max_steps_reached
type outcome = { status : status; summary : string; steps : int }

let max_parse_retries = 2
let max_history_chars = 60_000
let compact_threshold_chars = max_history_chars * 3 / 4
let compact_keep_recent_chunks = 6

let final_answer_nudge =
  "Tool budget exhausted. Do not call any more tools. Provide your best final \
   answer now based only on the context and tool observations already \
   available. If you use the fallback JSON format, return final_answer."

let status_to_string = function
  | Completed -> "completed"
  | Failed -> "failed"
  | Max_steps_reached -> "max_steps_reached"

let max_content_chars = 12_000

let clamp_string s =
  if String.length s <= max_content_chars then s
  else
    String.prefix s max_content_chars
    ^ Printf.sprintf "\n…[truncated %d chars]"
        (String.length s - max_content_chars)

let clamp_turn (turn : Llm.turn) =
  let clamp_content = function
    | Llm.Text s -> Llm.Text (clamp_string s)
    | Llm.Tool_result { id; content } ->
        Llm.Tool_result { id; content = clamp_string content }
    | other -> other
  in
  { turn with content = List.map turn.content ~f:clamp_content }

let turn_cost (turn : Llm.turn) =
  let content_cost = function
    | Llm.Text s -> String.length s
    | Llm.Thinking { text; signature } ->
        String.length text + String.length signature
    | Llm.Tool_use { name; input; id } ->
        String.length id + String.length name
        + String.length (Yojson.Safe.to_string input)
    | Llm.Tool_result { id; content } ->
        String.length id + String.length content
  in
  List.sum (module Int) turn.content ~f:content_cost + 16

let history_cost turns = List.sum (module Int) turns ~f:turn_cost

let tool_use_ids content =
  List.filter_map content ~f:(function
    | Llm.Tool_use { id; _ } -> Some id
    | _ -> None)

let is_tool_result_turn_for ids (turn : Llm.turn) =
  match turn.role with
  | Llm.Assistant -> false
  | Llm.User ->
      (not (List.is_empty turn.content))
      && List.for_all turn.content ~f:(function
        | Llm.Tool_result { id; _ } -> List.mem ids id ~equal:String.equal
        | _ -> false)

let is_orphan_tool_result_turn (turn : Llm.turn) =
  match turn.role with
  | Llm.Assistant -> false
  | Llm.User ->
      List.exists turn.content ~f:(function
        | Llm.Tool_result _ -> true
        | _ -> false)

let history_chunks turns =
  let rec loop acc = function
    | [] -> List.rev acc
    | ({ Llm.role = Llm.Assistant; content } as turn) :: rest -> (
        match tool_use_ids content with
        | [] -> loop ([ turn ] :: acc) rest
        | ids ->
            let rec collect results = function
              | result_turn :: tl when is_tool_result_turn_for ids result_turn
                ->
                  collect (result_turn :: results) tl
              | remaining -> (List.rev results, remaining)
            in
            let result_turns, remaining = collect [] rest in
            loop ((turn :: result_turns) :: acc) remaining)
    | turn :: rest when is_orphan_tool_result_turn turn -> loop acc rest
    | turn :: rest -> loop ([ turn ] :: acc) rest
  in
  loop [] turns

let split_recent_chunks chunks =
  let n = List.length chunks in
  if n <= compact_keep_recent_chunks + 1 then None
  else
    let older_count = n - compact_keep_recent_chunks in
    let older, recent = List.split_n chunks older_count in
    Some (List.concat older, List.concat recent)

let compact_summary_chars = 24_000
let compact_block_chars = 1_200

let compact_excerpt s =
  let s = String.strip s in
  if String.length s <= compact_block_chars then s
  else
    String.prefix s compact_block_chars
    ^ Printf.sprintf " …[omitted %d chars]"
        (String.length s - compact_block_chars)

let compact_summary_bound s =
  if String.length s <= compact_summary_chars then s
  else
    let head = String.prefix s 8_000 in
    let tail_len = compact_summary_chars - String.length head - 80 in
    let tail = String.suffix s tail_len in
    head ^ "\n\n…[middle of compacted context omitted]\n\n" ^ tail

let compact_text_of_turn (turn : Llm.turn) =
  let role =
    match turn.role with Llm.User -> "User" | Llm.Assistant -> "Assistant"
  in
  let block = function
    | Llm.Text s -> compact_excerpt s
    | Llm.Thinking _ -> ""
    | Llm.Tool_use { id; name; input } ->
        Printf.sprintf "[tool_call id=%s name=%s args=%s]" id name
          (Yojson.Safe.to_string input)
    | Llm.Tool_result { id; content } ->
        Printf.sprintf "[tool_result id=%s] %s" id (compact_excerpt content)
  in
  role ^ ": "
  ^ (List.filter_map turn.content ~f:(fun c ->
         let s = block c in
         if String.is_empty s then None else Some s)
    |> String.concat ~sep:"\n")

let compact_summary older =
  older
  |> List.map ~f:compact_text_of_turn
  |> String.concat ~sep:"\n\n" |> compact_summary_bound

(* Keep the most recent turns that fit the budget, so long sessions do not blow
   the context window. Tool exchanges must be kept or dropped as a unit;
   provider APIs reject orphaned tool_result blocks. *)
let truncate_history turns =
  let rec take acc budget = function
    | [] -> List.concat acc
    | chunk :: tl ->
        let cost = List.sum (module Int) chunk ~f:turn_cost in
        let clamped_chunk = List.map chunk ~f:clamp_turn in
        if budget - cost < 0 then
          if List.is_empty acc then
            if List.length chunk = 1 then List.concat [ clamped_chunk ] else []
          else List.concat acc
        else take (clamped_chunk :: acc) (budget - cost) tl
  in
  take [] max_history_chars (List.rev (history_chunks turns))

let max_history_chars_for_test = max_history_chars
let truncate_history_for_test = truncate_history
let compact_threshold_chars_for_test = compact_threshold_chars

let run ?(on_event = fun _ -> ()) ?(policy = Policy.default)
    ?(on_approval = fun _ _ -> Lwt.return false) ?(initial_history = [])
    ?(yolo = false) ~(config : Config.t) ~model_client ~event_log ~workspace
    ~task () =
  (* Single source of truth: live state is the fold of the events we emit, so
     it is identical by construction to replaying the log (resume/fork). *)
  let st = ref { Session_state.empty with turns = initial_history } in
  let emit event =
    Event_log.append event_log event;
    on_event event;
    st := Session_state.reduce !st event
  in
  let goto next =
    let from_state = Session_state.agent_state !st in
    let to_state =
      match Agent_state.transition from_state next with
      | Ok s -> s
      | Error _ -> next
    in
    emit (Event.State_transition { from_state; to_state })
  in
  let system = Model_client.system_prompt in
  let turns () = truncate_history (Session_state.turns !st) in
  let emit_delta content =
    if not (String.is_empty content) then
      on_event (Event.Model_delta { content })
  in
  let compact_if_needed () =
    let current = Session_state.turns !st in
    if history_cost current < compact_threshold_chars then Lwt.return_unit
    else
      match current |> history_chunks |> split_recent_chunks with
      | None -> Lwt.return_unit
      | Some (older, recent) ->
          let summary = compact_summary older in
          if String.is_empty summary then Lwt.return_unit
          else (
            emit (Event.Context_compacted { summary; recent });
            Lwt.return_unit)
  in
  emit (Event.User_message { content = task });
  goto Agent_state.Waiting_for_model;
  let finish status summary steps = Lwt.return { status; summary; steps } in
  let rec step n =
    if n > config.max_steps then finalize_after_tool_budget ()
    else send_with_retry 0 n
  and finalize_after_tool_budget () =
    emit (Event.User_message { content = final_answer_nudge });
    Lwt.bind
      (Model_client.send model_client ~on_delta:emit_delta ~tools_enabled:false
         ~system ~turns:(turns ()))
      (function
        | Error e ->
            goto Agent_state.Failed;
            finish Max_steps_reached
              (Printf.sprintf
                 "reached the maximum step limit and finalization failed: %s" e)
              (Session_state.steps !st)
        | Ok (content, usage) -> (
            emit (Event.Assistant_message { content; usage });
            match Llm.final_text content with
            | Some answer ->
                goto Agent_state.Completed;
                finish Completed answer (Session_state.steps !st)
            | None ->
                goto Agent_state.Failed;
                finish Max_steps_reached
                  "reached the maximum step limit without producing a final \
                   answer"
                  (Session_state.steps !st)))
  and send_with_retry retries n =
    Lwt.bind (compact_if_needed ()) (fun () ->
        Lwt.bind
          (Model_client.send model_client ~on_delta:emit_delta ~system
             ~turns:(turns ()))
          (fun result ->
            match result with
            | Error e ->
                if retries < max_parse_retries then (
                  (* the nudge is a real conversation message, so it is emitted as
                 an event and folded into state like everything else *)
                  emit
                    (Event.User_message
                       {
                         content =
                           Printf.sprintf
                             "Your previous reply could not be processed (%s). \
                              Reply with a SINGLE valid JSON action."
                             e;
                       });
                  send_with_retry (retries + 1) n)
                else (
                  goto Agent_state.Failed;
                  finish Failed ("model interaction failed: " ^ e) (n - 1))
            | Ok (content, usage) ->
                emit (Event.Assistant_message { content; usage });
                handle_content content n))
  and handle_content content n =
    let denied_result permission =
      match permission with
      | Permission.Deny reason ->
          Tool_result.Error { message = "policy denied: " ^ reason }
      | Permission.Ask_user reason ->
          Tool_result.Error
            { message = "requires user approval (not supported): " ^ reason }
      | Permission.Allow ->
          Tool_result.Error { message = "internal error: expected denial" }
    in
    let prepare_tool_call (id, tc) =
      emit (Event.Tool_call tc);
      let permission = Policy.check ~yolo ~workspace ~tool_call:tc () in
      emit (Event.Policy_decision { tool_call = tc; permission });
      if not (Permission.is_allow permission) then
        Lwt.return (id, fun () -> Lwt.return (denied_result permission))
      else
        match Policy.approval_reason policy tc with
        | None ->
            Lwt.return
              ( id,
                fun () -> Tool_runner.run_lwt ~yolo ~workspace ~tool_call:tc ()
              )
        | Some reason ->
            Lwt.bind (on_approval tc reason) (fun approved ->
                if approved then
                  Lwt.return
                    ( id,
                      fun () ->
                        Tool_runner.run_lwt ~yolo ~workspace ~tool_call:tc () )
                else
                  Lwt.return
                    ( id,
                      fun () ->
                        Lwt.return
                          (Tool_result.Error
                             { message = "user did not approve: " ^ reason }) ))
    in
    let after_results results =
      (* reduce derives observation messages from these events. Results are
         emitted in request order even if execution completed out of order. *)
      List.iter results ~f:(fun (id, result) ->
          emit (Event.Tool_result_message { id; result }));
      goto Agent_state.Observing_result;
      goto Agent_state.Waiting_for_model;
      step (n + 1)
    in
    let execute_batch calls =
      match calls with
      | [] ->
          goto Agent_state.Failed;
          finish Failed "model returned an empty tool call batch" n
      | calls ->
          goto Agent_state.Executing_tool;
          Lwt.bind (Lwt_list.map_s prepare_tool_call calls) (fun runners ->
              Lwt.bind
                (Lwt.all
                   (List.map runners ~f:(fun (id, run) ->
                        Lwt.map (fun result -> (id, result)) (run ()))))
                after_results)
    in
    match Llm.tool_uses content with
    | _ :: _ as calls -> execute_batch calls
    | [] -> (
        match Llm.final_text content with
        | Some answer ->
            goto Agent_state.Completed;
            finish Completed answer n
        | None ->
            goto Agent_state.Failed;
            finish Failed "model returned no final text or tool calls" n)
  in
  step 1
