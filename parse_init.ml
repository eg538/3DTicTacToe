open Yojson.Basic.Util
open Types
(* type level = Easy| Medium| Hard

type player = Caml | Python| None

type num_players = Single| Multi

type info = {
  info_mode : num_players;
  info_p1_avatar : player;
  info_level  : level;
} *)

let parse_init_file file =
  let init = Yojson.Basic.from_file file |> member "init" in
  {
  info_mode =
    if (member "mode" init |> to_string) = "single" then
      Single
    else Multi;
  info_p1_avatar =
    if (member "p1_avatar" init |> to_string) = "python" then
      Python
    else Caml;
  info_level =
    if (member "level" init |> to_string) = "easy" then
      Easy
    else if (member "level" init |> to_string) = "medium" then
      Medium
    else
      Hard
  }

let mode i = i.info_mode

let p1_avatar i = i.info_p1_avatar

let level i = i.info_level
