open! Base

type status = Completed | Failed | Max_steps_reached
type outcome = { status : status; summary : string; steps : int }

let max_parse_retries = 2
let max_history_chars = 60_000
let compact_threshold_chars = max_history_chars * 3 / 4
let compact_keep_recent_chunks = 6
let review_max_steps = 14

let final_answer_nudge =
  "Tool budget exhausted. Do not call any more tools. Provide your best final \
   answer now based only on the context and tool observations already \
   available. If you use the fallback JSON format, return final_answer."

let is_code_review_task task =
  let s = String.lowercase task in
  String.is_substring s ~substring:"code review"
  || String.equal (String.strip s) "review"
  || String.is_substring s ~substring:"review this"
  || String.is_substring s ~substring:"review the"
  || String.is_substring s ~substring:"review my"
  || String.is_substring s ~substring:"review for"
  || String.is_substring s ~substring:"审查"
  || String.is_substring s ~substring:"代码 review"

let review_task_guidance =
  "Code review mode: find correctness, safety, regression, and maintainability \
   issues in the current changes. Do not produce an architecture overview as \
   the final answer. Start by inspecting git status --short and git diff \
   --stat. Then inspect only changed files/diffs and directly related code. \
   Prefer git diff <path> over reading whole files. Batch independent \
   read-only commands when possible. Stop once you have enough evidence; final \
   output must be a concise findings list with severity, file path/line \
   evidence, tests run, and any residual risks. If there are no findings, say \
   so and cite what you checked."

let system_for_task ~workspace task =
  let base =
    if is_code_review_task task then
      Model_client.system_prompt ^ "\n\n" ^ review_task_guidance
    else Model_client.system_prompt
  in
  match Project_instructions.load workspace with
  | None -> base
  | Some instructions -> base ^ "\n\n" ^ instructions

let review_final_answer_nudge =
  "Review budget exhausted. Do not call any more tools. Produce a code review \
   now, not an onboarding document or architecture overview. Use this format: \
   Findings; Tests run; Residual risks. Each finding must include severity, \
   file/path evidence, and a concrete reason. If there are no findings, say no \
   findings and list the diffs/files checked."

let bad_review_answer answer =
  let s = String.lowercase answer in
  (String.is_substring s ~substring:"codebase onboarding document"
  || String.is_substring s ~substring:"high-level architecture"
  || String.is_substring s ~substring:"repository layout")
  && not (String.is_substring s ~substring:"finding")

let review_preflight workspace =
  let root = Workspace.root workspace in
  let command =
    Printf.sprintf
      "cd %s && echo '--- git status --short ---' && git status --short && \
       echo '--- git diff --stat ---' && git diff --stat && echo '--- git diff \
       --cached --stat ---' && git diff --cached --stat"
      (Stdlib.Filename.quote root)
  in
  match Shell.run ~command ~timeout_sec:10 with
  | Error e -> "[Code review preflight failed]\n" ^ e
  | Ok { stdout; stderr; exit_code } ->
      Printf.sprintf
        "[Code review preflight]\n\
         exit_code=%d\n\
         %s%s\n\n\
         Use this preflight to scope the review. Inspect changed diffs next; \
         do not write an architecture overview."
        exit_code stdout
        (if String.is_empty stderr then "" else "\n--- stderr ---\n" ^ stderr)

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

let update_plan_tool_name = "update_plan"

let is_update_plan_tool (tc : Tool_call.t) =
  String.equal tc.name update_plan_tool_name

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

let compact_event_of_turns turns =
  match turns |> history_chunks |> split_recent_chunks with
  | None -> None
  | Some (older, recent) ->
      let summary = compact_summary older in
      if String.is_empty summary then None
      else Some (Event.Context_compacted { summary; recent })

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
  let review_task = is_code_review_task task in
  let system = system_for_task ~workspace task in
  let effective_max_steps =
    if review_task then Int.min config.max_steps review_max_steps
    else config.max_steps
  in
  let final_nudge =
    if review_task then review_final_answer_nudge else final_answer_nudge
  in
  let turns () = truncate_history (Session_state.turns !st) in
  let emit_delta content =
    if not (String.is_empty content) then
      on_event (Event.Model_delta { content })
  in
  let compact_if_needed () =
    let current = Session_state.turns !st in
    if history_cost current < compact_threshold_chars then Lwt.return_unit
    else
      match compact_event_of_turns current with
      | None -> Lwt.return_unit
      | Some event ->
          emit event;
          Lwt.return_unit
  in
  emit (Event.User_message { content = task });
  if review_task then
    emit (Event.User_message { content = review_preflight workspace });
  goto Agent_state.Waiting_for_model;
  let finish status summary steps = Lwt.return { status; summary; steps } in
  let rec step n =
    if n > effective_max_steps then finalize_after_tool_budget ()
    else send_with_retry 0 n
  and finalize_after_tool_budget () =
    emit (Event.User_message { content = final_nudge });
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
                finish Max_steps_reached answer (Session_state.steps !st)
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
    let update_plan_result tc =
      match Event.plan_items_of_json tc.Tool_call.args with
      | Error message -> Tool_result.Error { message }
      | Ok items ->
          emit (Event.Plan_updated { items });
          Tool_result.Success
            {
              output =
                Printf.sprintf "plan updated: %d item(s)" (List.length items);
            }
    in
    let prepare_tool_call (id, tc) =
      emit (Event.Tool_call tc);
      let permission = Policy.check ~yolo ~workspace ~tool_call:tc () in
      emit (Event.Policy_decision { tool_call = tc; permission });
      if not (Permission.is_allow permission) then
        Lwt.return (id, fun () -> Lwt.return (denied_result permission))
      else if is_update_plan_tool tc then
        Lwt.return (id, fun () -> Lwt.return (update_plan_result tc))
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
        | Some answer when review_task && bad_review_answer answer ->
            emit
              (Event.User_message
                 {
                   content =
                     "That response is not a code review. Do not provide an \
                      onboarding document, architecture summary, repository \
                      layout, or API overview. Produce only code-review \
                      findings with severity, file/path evidence, tests run, \
                      and residual risks. If there are no findings, say no \
                      findings and cite the exact diffs/files checked.";
                 });
            step (n + 1)
        | Some answer ->
            goto Agent_state.Completed;
            finish Completed answer n
        | None ->
            goto Agent_state.Failed;
            finish Failed "model returned no final text or tool calls" n)
  in
  step 1
