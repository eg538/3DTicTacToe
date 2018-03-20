open Command
type info
type state
type player

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
