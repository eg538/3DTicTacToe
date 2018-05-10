type level = Easy| Medium| Hard

type player = Caml | Python| None

type num_players = Single| Multi

type info = {
  info_mode : num_players;
  info_p1_avatar : player;
  info_level  : level;
}

type cell = {cell: (int*int*int); player: player}

type winType3D =
  | WinV of cell list list
  | WinH of cell list list
  | WinNone

type board = ((int*int*int), cell) Hashtbl.t

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
  diagonals: cell list list;
}

type command =
  |Play of string
  |Score
  |Quit
  |Restart
  |Try of (int * int * int)
  |Place of (int * int * int)
  |Hint
  |Look
  | CurrentPlayer
  |Invalid
(*open State
open Grid_3d
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
  print_endline "adding on a diag 3 level win";
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

let updating st (pl,row,col) =
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
*)
