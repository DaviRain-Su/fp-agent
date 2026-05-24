open! Base

type status = Completed | Failed | Max_steps_reached
type outcome = { status : status; summary : string; steps : int }

let max_parse_retries = 2
let max_history_chars = 60_000

let status_to_string = function
  | Completed -> "completed"
  | Failed -> "failed"
  | Max_steps_reached -> "max_steps_reached"

(* Keep the system prompt plus the most recent messages that fit the budget, so
   long sessions do not blow the context window. *)
let truncate_history ~system messages =
  let rec take acc budget = function
    | [] -> acc
    | (m : Message.t) :: tl ->
        let cost = String.length m.content + 16 in
        if budget - cost < 0 then acc else take (m :: acc) (budget - cost) tl
  in
  system :: take [] max_history_chars (List.rev messages)

let run ?(on_event = fun _ -> ()) ?(policy = Policy.default)
    ?(on_approval = fun _ _ -> Lwt.return false) ?(initial_history = [])
    ?(yolo = false) ~(config : Config.t) ~model_client ~event_log ~workspace
    ~task () =
  (* Single source of truth: live state is the fold of the events we emit, so
     it is identical by construction to replaying the log (resume/fork). *)
  let st = ref { Session_state.empty with messages = initial_history } in
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
  let system = Message.system Model_client.system_prompt in
  let messages () = truncate_history ~system (Session_state.messages !st) in
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
      (Model_client.send model_client ~messages:(messages ()))
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
        | Ok action ->
            (* reduce derives the assistant message from this event *)
            emit (Event.Model_response { action });
            handle_action action n)
  and handle_action action n =
    match (action : Model_action.t) with
    | Final_answer { answer } ->
        goto Agent_state.Completed;
        finish Completed answer n
    | Tool_call tc -> (
        goto Agent_state.Executing_tool;
        emit (Event.Tool_call tc);
        let permission = Policy.check ~yolo ~workspace ~tool_call:tc () in
        emit (Event.Policy_decision { tool_call = tc; permission });
        let after_result result =
          (* reduce derives the observation message from this event *)
          emit (Event.Tool_result result);
          goto Agent_state.Observing_result;
          goto Agent_state.Waiting_for_model;
          step (n + 1)
        in
        let execute () =
          after_result (Tool_runner.run ~yolo ~workspace ~tool_call:tc ())
        in
        (* Deny is enforced by the runner; for allowed calls, gate risky ones
           on human approval when the policy asks for it. *)
        if not (Permission.is_allow permission) then execute ()
        else
          match Policy.approval_reason policy tc with
          | None -> execute ()
          | Some reason ->
              Lwt.bind (on_approval tc reason) (fun approved ->
                  if approved then execute ()
                  else
                    after_result
                      (Tool_result.Error
                         { message = "user did not approve: " ^ reason })))
  in
  step 1
