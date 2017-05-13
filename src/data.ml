type ws_event = {
  id: int;
  type': string;
  channel: string;
  user: string;
}

type feedback = KillMeNow of (unit -> unit) | Simple of string

type message =
  Pong
  | Msg of (string * string)
  | Other of string

let to_json event =
  let open Yojson.Basic in
  let json = `Assoc [
    ("id", `Int event.id);
    ("type", `String event.type');
    ("channel", `String event.channel);
    ("user", `String event.user)
  ] in
  to_string json

let of_string event =
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  let open Core.Std in
  try
    let value = Option.value in
    let json = from_string event in
    let type' = value (json |> member "type" |> to_string_option) ~default:"" in
    let inner_data =
      match type' with
      | "pong" ->
          Pong
      | "message" ->
          let text = value (json |> member "text" |> to_string_option) ~default:"" in
          let user = value (json |> member "user" |> to_string_option) ~default:"" in
          Msg (text, user)
      | other ->
          Other other in
    Some inner_data
  with
    Yojson.Json_error _ -> None
