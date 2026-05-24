open! Base

type status = Completed | Failed | Max_steps_reached
type outcome = { status : status; summary : string; steps : int }

let max_parse_retries = 2
let max_history_chars = 60_000

let status_to_string = function
  | Completed -> "completed"
  | Failed -> "failed"
  | Max_steps_reached -> "max_steps_reached"

(* Keep the most recent turns that fit the budget, so
   long sessions do not blow the context window. *)
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

let truncate_history turns =
  let rec take acc budget = function
    | [] -> acc
    | turn :: tl ->
        let cost = turn_cost turn in
        if budget - cost < 0 then acc else take (turn :: acc) (budget - cost) tl
  in
  take [] max_history_chars (List.rev turns)

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
  emit (Event.User_message { content = task });
  goto Agent_state.Waiting_for_model;
  let finish status summary steps = Lwt.return { status; summary; steps } in
  let rec step n =
    if n > config.max_steps then (
      goto Agent_state.Failed;
      finish Max_steps_reached
        "reached the maximum step limit without producing a final answer" (n - 1))
    else send_with_retry 0 n
  and send_with_retry retries n =
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
            handle_content content n)
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
