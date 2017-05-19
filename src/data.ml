module Event = struct
  type t =
    Ping of int |
    Pong of int |
    Msg of (int * string * string)

  let to_json e =
    let open Yojson.Basic in
    let json =
      match e with
      | Ping id ->
          `Assoc [
            ("id", `Int id);
            ("type", `String "ping")
          ]
      | Pong id ->
          `Assoc [
            ("id", `Int id);
            ("type", `String "pong")
          ]
      | Msg (id, text, user) ->
          `Assoc [
            ("id", `Int id);
            ("type", `String "message");
            ("text", `String text);
            ("user", `String user)
          ]
    in
    to_string json

  let of_json event =
    let open Core.Std.Option in
    let member = Yojson.Basic.Util.member in
    let to_string_option = Yojson.Basic.Util.to_string_option in
    let to_int_option = Yojson.Basic.Util.to_int_option in
    let from_string = Yojson.Basic.from_string in
    let try_with = Core.Std.Option.try_with in
    (try_with (fun () -> from_string event)) >>= (fun json ->
      (json |> member "type" |> to_string_option) >>= (fun type' ->
        let id = value (json |> member "id" |> to_int_option) ~default:0 in
        match type' with
        | "ping" ->
            Some (Ping id)
        | "pong" ->
            Some (Pong id)
        | "message" ->
            let text = value (json |> member "text" |> to_string_option) ~default:"" in
            let user = value (json |> member "user" |> to_string_option) ~default:"" in
            Some (Msg (id, text, user))
        | _ -> None))

  let ping () =
    Ping (Random.int 999999)
end

module Feedback = struct
  type t =
    KillMeNow of (unit -> unit) |
    Simple of string |
    Retry of string
end
