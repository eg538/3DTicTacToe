open Command
open Grid_3d

type level = Easy| Medium| Hard

type player = Caml | Python

type num_players = Single| Multi

type result = Win| Lose| Draw| None

type info = {
  avatar : player;
  level  : level;
  mode   : num_players;
}

type state = {
  result  : result;
  cells   : cell list;
  most_recent_player  : player;
  curr_score_1 : int;
  curr_score_2 : int;
  mode  : num_players;
  level : level;
  wim_message  : string;
  loss_message : string;
  draw_message : string;
  num_hints : int;
  num_tries : int;
}

module type State = sig
  val init_state : info -> state
  val p1_score: state -> int
  val p2_score : state -> int
  val curr_player : state -> string
  val asciiBoard: state -> string
  val hint : state -> string
  val board : state -> string
  val avatars : state -> string * player list
  val do' : command -> state -> state
end

module State: State = struct
  let init_state = failwith "Unimplemented"
  let p1_score = failwith "Unimplemented"
  let p2_score = failwith "Unimplemented"
  let curr_player = failwith "Unimplemented"
  let asciiBoard = failwith "Unimplemented"
  let hint = failwith "Unimplemented"
  let board = failwith "Unimplemented"
  let avatars = failwith "Unimplemented"
  let do' = failwith "Unimplemented"

end
