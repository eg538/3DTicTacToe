open Command
open Grid_3d
open Parse_init
open Types

let num_helper lvl =
  match lvl with
  |Easy -> 7
  |Medium -> 5
  |Hard -> 3

let init_state str =
  let i = parse_init str in
  {
    tttBoard = copy empty_board;
    current_player = Python;
    curr_score_1 = 0;
    curr_score_2 = 0;
    mode = mode i;
    num_players = player_num i;
    level = level i;
    p1_avatar = p1_avatar i;
    p1_num_hints = num_helper (level i);
    p1_num_tries = num_helper (level i);
    p2_num_hints = num_helper (level i);
    p2_num_tries = num_helper (level i);
    diagonals = [];
    winner = None;
    game_end = false
  }

let game_mode s = s.mode

let game_num_plyrs s = s.num_players

let game_level s = s.level

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

let get_result s = s.winner

let get_result_message s =
  match s.num_players with
  | Multi -> begin
    match s.winner with
    | Caml -> "Caml wins!"
    | Python -> "Python wins!"
    | None -> "Draw! No one won"
    end
  | Single -> if s.winner = s.p1_avatar then
              "Congratulations! You won the Java cup!"
            else if s.winner <> None then
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

let p1_avatar s = s.p1_avatar

(*[inc_point st] increments the score of the current player of state [st]*)
let inc_point st =
  if (st.p1_avatar = Python && st.current_player = Python) || (st.p1_avatar = Caml && st.current_player = Caml) then
    {st with curr_score_1 = st.curr_score_1 + 1}
  else
    {st with curr_score_2 = st.curr_score_2 + 1}

 let rec extract_cells_from_st st st_diags =
  match st.diagonals with
  | [] -> []
  | [{cell=c1;player=_};{cell=c2;player=_};{cell=c3;player=_}]::t -> [c1;c2;c3]::(extract_cells_from_st st t)
  | _ -> []

let diag_cells_in_question diag_list =
  match diag_list with
  | [] -> []
  | (h1:cell)::h2::h3::[] -> (h1.cell)::(h2.cell)::(h3.cell)::[]
  | _ -> []

let rec check diag_cells_in_question h =
  match diag_cells_in_question with
  | [] -> true
  | head::tail -> (head=h) && (check tail h)
  | _ -> false

let rec search st diag_cells_in_q diags_cell_list_only = (*let diags_cell_list_only = extract_cells_from_st st.diagonals*)
  match (diags_cell_list_only:(int*int*int) list list) with
  | h::t -> (check diag_cells_in_q h) || (search st diag_cells_in_q t)
  | _ -> false

  (*match (diags_cell_list_only: ((int*int*int) list list)) with
  | [] -> false
    | h::t -> (check diag_cells_in_question h) || (search st diag_cells_in_question t) *)

let accumulate_diag_wins diag_list st = (*cell list list *)
  (* print_endline "adding on a diag 3 level win"; *)
  let s = extract_cells_from_st st st.diagonals in (*(int int int) list list *)
  match (diag_list: cell list list) with
  | [] -> st
  | (h:cell list)::[] ->
    begin
      let d_cells = [diag_cells_in_question h] in (*int int int list*)
      if (search st (d_cells) s) then st else {st with diagonals = h :: st.diagonals}
    end
  | (h1:cell list)::h2::[] ->
    begin
      let h1' = [diag_cells_in_question h1] in
      let h2' = [diag_cells_in_question h2] in
      if (not (search st h1' s)) && (not (search st h2' s)) then {st with diagonals = h1 :: h2 :: st.diagonals}
      else if not (search st h1' s) then {st with diagonals = h1 :: st.diagonals}
      else if not (search st h2' s) then {st with diagonals = h2 :: st.diagonals}
      else st
    end
  | _ -> failwith "impossible"

(*[play_move st coords] is the new state as a result of the current player of state [st]
 * making a move at coordinates [coords], updating the scores accordingly if the move
 * creates a three-in-a-row for the player that made the move*)
let play_move st (pl, row, col) =
  make_move st (pl, row, col);
<<<<<<< HEAD
  print_board st;
=======
>>>>>>> ai
  if win_evaluation (find_cell st (pl, row, col)) st.tttBoard then
    (* updating st (pl, row, col) *)
    begin
    let c = find_cell st (pl,row,col) in
    let b = st.tttBoard in
    let diag_check_truth = (((diag_check c b)|> fst) <> WinNone) || (((diag_check c b) |> snd) <> WinNone) in
    let instances = (three_row_2d_cells c b) in
    let case_2d = victory_on_plane c instances in
    let case_3d = (diag_check_truth) || (col_check c b) in
    match (case_2d, case_3d) with
      | true, true ->
        begin
          inc_point st;
          let st' = (accumulate_diag_wins (get_the_win (find_cell st (pl,row,col)) st.current_player st.tttBoard) st) in (*cell list list*)
          if (st'.diagonals = st.diagonals) then st else inc_point st
        end
      | true, false ->
        begin
          inc_point st
        end
      | false, true ->
        begin
          let st' = (accumulate_diag_wins (get_the_win (find_cell st (pl,row,col)) st.current_player st.tttBoard) st) in
          if (st'.diagonals = st.diagonals) then st else inc_point st
        end
      | _ -> st
       end
  else
    st

let other_player ply = 
  match ply with
  | Python -> Caml
  | Caml -> Python
  | None -> None

(*[switch_players st] returns the opponent of the current player of state [st]*)
let switch_players st = let _p1_av = st.p1_avatar in
match st.current_player with
| Python -> {st with current_player = Caml}
| _ -> {st with current_player = Python}

let check_game_end st = 
  if cells_left st.tttBoard = [] then 
    if st.curr_score_1 > st.curr_score_2 then
      {st with winner = st.p1_avatar; game_end = true}
    else if st.curr_score_2 > st.curr_score_1 then
      {st with winner = other_player st.p1_avatar; game_end = true}
    else
      {st with game_end = true}
  else
    st

let game_ended st = st.game_end

         (*
let empty_diags =
  (*horizontal_3d *)
  let d1 = [{cell=(0,0,0);player=None};{cell=(1,0,1);player=None;{cell=(2,0,2);player=None}] in
  let d2 = [{cell=(0,0,2);player=None};{cell=(1,0,1);player=None};{cell=(2,0,0);player=None}] in
  let d3 = [{cell=(0,2,0);player=None};{cell=(1,2,1);player=None};{cell=(2,2,2);player=None}] in
  let d4 = [{cell=(0,2,2);player=None};{cell=(1,2,1);player=None};{cell=(2,2,0);player=None}] in
  (*vertical_3d *)
  let d5 = [{cell=(0,0,0);player=None};{cell=(1,1,0);player=None};{cell=(2,2,0);player=None}] in
  let d6 = [{cell=(2,0,0);player=None};{cell=(1,1,0);player=None};{cell=(0,0,2);player=None}] in
  let d7 = [{cell=(0,0,2);player=None};{cell=(1,1,2);player=None};{cell=(2,2,2);player=None}] in
  let d8 = [{cell=(2,0,2);player=None};{cell=(1,1,2);player=None};{cell=(0,2,2);player=None}] in
  d1::d2::d3::d4::d5::d6::d7::d8::[]
*)
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
    (* print_endline ("Move made: "^(string_of_int pl)^", "^(string_of_int row)^", "^(string_of_int col)); *)
    begin
      try(
<<<<<<< HEAD
        play_move st (pl, row, col) |> switch_players;
        (*if (mode<>"normal") then krazy_mode_bomb c st.tttBoard else ();*)
=======
        play_move st (pl, row, col) |> switch_players |> check_game_end
>>>>>>> ai
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
