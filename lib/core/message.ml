open! Base

type t = { role : string; content : string }

let system content = { role = "system"; content }
let user content = { role = "user"; content }
let assistant content = { role = "assistant"; content }
