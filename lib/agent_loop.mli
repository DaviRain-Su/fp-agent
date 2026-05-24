type status = Completed | Failed | Max_steps_reached
type outcome = { status : status; summary : string; steps : int }

val status_to_string : status -> string

val run :
  config:Config.t ->
  model_client:Model_client.t ->
  event_log:Event_log.t ->
  workspace:Workspace.t ->
  task:string ->
  outcome Lwt.t
(** Drive the agent: send the task to the model, execute tool calls behind the
    policy, feed observations back, and log every step. Stops on a final answer,
    an unrecoverable model error, or the step limit. *)
