open Yojson.Basic.Util
open Types

(* type level = Easy| Medium| Hard
type player = Caml | Python| None
type num_players = Single| Multi
type info  *)

(*[parse_init_file f] is the information extract from json file [f]*)
val parse_init_file: string -> info

(*[mode i] is the number of players for the game as specified by [i]*)
val mode: info -> num_players

(*[p1_avatar i] is the avatar of player 1 as specified by [i]*)
val p1_avatar: info -> player

(*[level i] is the level of the game as specified by [i]*)
val level: info -> level
