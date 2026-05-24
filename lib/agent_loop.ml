open! Base

type status = Completed | Failed | Max_steps_reached
type outcome = { status : status; summary : string; steps : int }

let max_parse_retries = 2
let max_history_chars = 60_000

let status_to_string = function
  | Completed -> "completed"
  | Failed -> "failed"
  | Max_steps_reached -> "max_steps_reached"

let observation_of_result (result : Tool_result.t) =
  match result with
  | Tool_result.Success { output } -> "TOOL_RESULT ok=true\n" ^ output
  | Tool_result.Error { message } -> "TOOL_RESULT ok=false\n" ^ message

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

let run ~(config : Config.t) ~model_client ~event_log ~workspace ~task =
  let log e = Event_log.append event_log e in
  let state = ref Agent_state.Initializing in
  let goto next =
    match Agent_state.transition !state next with
    | Ok s ->
        log (Event.State_transition { from_state = !state; to_state = s });
        state := s
    | Error _ -> state := next
  in
  log (Event.User_message { content = task });
  let history = ref [ Message.user task ] in
  let add_msg m = history := !history @ [ m ] in
  let system = Message.system Model_client.system_prompt in
  let messages () = truncate_history ~system !history in
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
              add_msg
                (Message.user
                   (Printf.sprintf
                      "Your previous reply could not be processed (%s). Reply \
                       with a SINGLE valid JSON action."
                      e));
              send_with_retry (retries + 1) n)
            else (
              goto Agent_state.Failed;
              finish Failed ("model interaction failed: " ^ e) (n - 1))
        | Ok action ->
            log (Event.Model_response { action });
            add_msg
              (Message.assistant
                 (Yojson.Safe.to_string (Model_action.to_yojson action)));
            handle_action action n)
  and handle_action action n =
    match (action : Model_action.t) with
    | Final_answer { answer } ->
        goto Agent_state.Completed;
        finish Completed answer n
    | Tool_call tc ->
        goto Agent_state.Executing_tool;
        log (Event.Tool_call tc);
        let result = Tool_runner.run ~workspace ~tool_call:tc in
        log (Event.Tool_result result);
        goto Agent_state.Observing_result;
        add_msg (Message.user (observation_of_result result));
        goto Agent_state.Waiting_for_model;
        step (n + 1)
  in
  step 1
