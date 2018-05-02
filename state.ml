open Command
open Grid_3d
open Parse_init

type state = {
  result  : player; (*Will contain the player who won*)
  tttBoard   : board;
  current_player  : player;
  curr_score_1 : int;
  curr_score_2 : int;
  mode  : num_players;
  level : level;
  p1_avatar : player;
  p1_num_hints : int;
  p1_num_tries : int;
  p2_num_hints : int;
  p2_num_tries : int;
}

(*[num_helper lvl] is the number of hints and tries that each player gets according
 * to the level they chose*)
let num_helper lvl =
  match lvl with
  |Easy -> 7
  |Medium -> 5
  |Hard -> 3

let init_state fl =
  let i = parse_init_file fl in
  {
    result = None;
    tttBoard = copy empty_board;
    current_player = Python;
    curr_score_1 = 0;
    curr_score_2 = 0;
    mode = mode i;
    level = level i;
    p1_avatar = p1_avatar i;
    p1_num_hints = num_helper (level i);
    p1_num_tries = num_helper (level i);
    p2_num_hints = num_helper (level i);
    p2_num_tries = num_helper (level i)
  }

let p1_score s = s.curr_score_1

let p2_score s = s.curr_score_2

let curr_player s = s.current_player

let num_hints s =
  match s.current_player with
  | p1_avatar -> s.p1_num_hints
  | _ -> s.p2_num_hints

let num_tries s = let check = s.p1_avatar in
  match s.current_player with
  | check -> s.p1_num_tries
  | _ -> s.p2_num_tries

let get_result s = s.result

let get_result_message s =
  match s.mode with
  | Single -> begin
    match s.result with
    | Caml -> "Caml wins!"
    | Python -> "Python wins!"
    | None -> "No one won"
  end
  | Multi -> if s.result = s.p1_avatar then
              "Congratulations! You won the Java cup!"
            else if s.result <> None then
              "Sad! You didn't win the Java cup, but try again next time for that steaming mug of Java!"
            else
              "Oh no! You were close to winning the Java cup!"

let rec find_cell s (pl, x, y) = get_cell (pl, x, y) s.tttBoard

let print_board st = print_string (asciiBoard st.tttBoard)

(*[make_move s coords] places a move at the cell at coordinates [coords]
 * for the current player of state [s]*)
let make_move s (pl, x, y) = place (pl, x, y) s.tttBoard s.current_player

let hint s = failwith "Unimplemented"

let board s = s.tttBoard

let avatars s =
  match s.p1_avatar with
  | Caml -> [("player1", Caml); ("player2", Python)]
  | Python -> [("player1", Python); ("player2", Caml)]
  | None -> []

(*[inc_point st] increments the score of the current player of state [st]*)
let inc_point st =
  print_endline "win!";
  if (st.p1_avatar = Python && st.current_player = Python) || (st.p1_avatar = Caml && st.current_player = Caml) then
    {st with curr_score_1 = st.curr_score_1 + 1}
  else
    {st with curr_score_2 = st.curr_score_2 + 1}

(*[play_move st coords] is the new state as a result of the current player of state [st]
 * making a move at coordinates [coords], updating the scores accordingly if the move 
 * creates a three-in-a-row for the player that made the move*)
let play_move st (pl, row, col) =
  make_move st (pl, row, col);
  print_endline "placed move";
  if win_evaluation (find_cell st (pl, row, col)) st.tttBoard then
    inc_point st
  else
    st

(*[switch_players st] returns the opponent of the current player of state [st]*)
let switch_players st = let _p1_av = st.p1_avatar in
match st.current_player with
| Python -> {st with current_player = Caml}
| _ -> {st with current_player = Python}

let do' c st =
  match c with
  | Play str -> st
  | Score -> st
  | Quit -> st
  | Restart -> st
  | Try (pl, row, col) ->
    if (st.p1_avatar = Python && st.current_player = Python) || (st.p1_avatar = Caml && st.current_player = Caml) then
      {st with p1_num_tries = if st.p1_num_tries > 0 then st.p1_num_tries - 1 else 0}
    else
      {st with p2_num_tries = if st.p2_num_tries > 0 then st.p2_num_tries - 1 else 0}
  | Place (pl, row, col) ->
    begin
      try(
        play_move st (pl, row, col) |> switch_players
      )with
      | _ -> st
    end
  | Hint ->
    if (st.p1_avatar = Python && st.current_player = Python) || (st.p1_avatar = Caml && st.current_player = Caml) then
      {st with p1_num_hints = if st.p1_num_hints > 0 then st.p1_num_hints - 1 else 0}
    else
      {st with p2_num_hints = if st.p2_num_hints > 0 then st.p2_num_hints - 1 else 0}
  | Look -> st
  | CurrentPlayer -> st
  | Invalid -> st
