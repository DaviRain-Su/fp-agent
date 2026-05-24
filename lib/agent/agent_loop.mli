type status = Completed | Failed | Max_steps_reached
type outcome = { status : status; summary : string; steps : int }

val status_to_string : status -> string
val max_history_chars_for_test : int
val truncate_history_for_test : Llm.turn list -> Llm.turn list

val run :
  ?on_event:(Event.t -> unit) ->
  ?policy:Policy.t ->
  ?on_approval:(Tool_call.t -> string -> bool Lwt.t) ->
  ?initial_history:Llm.turn list ->
  ?yolo:bool ->
  config:Config.t ->
  model_client:Model_client.t ->
  event_log:Event_log.t ->
  workspace:Workspace.t ->
  task:string ->
  unit ->
  outcome Lwt.t
(** Drive the agent: send the task to the model, execute tool calls behind the
    policy, feed observations back, and log every step. Stops on a final answer,
    an unrecoverable model error, or the step limit. *)
