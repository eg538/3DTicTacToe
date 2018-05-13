open Types
open Grid_3d
<<<<<<< HEAD
open State

let krazy_disappearing_sqs st c =
  if (c=st.k_disappearing_sqs) then (
    Hashtbl.replace st.tttBoard c.cell {c with player = None}
  ) else ()

=======
open State 

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "python"
  | Caml -> "caml"
  | None -> "none"

let string_of_mode m = match m with
  | Normal -> "normal"
  | Krazy -> "krazy"

let string_of_num_p n = match n with 
  | Single -> "single"
  | Multi -> "multi"

let string_of_level l = match l with
  | Easy -> "easy"
  | Medium -> "medium"
  | Hard -> "hard"

>>>>>>> e7ab3798b73c533ffc491bc1e9c651b96d36585b
let rec krazy_recalc_helper cellst st =
  match cellst with
  | [] -> st
  | h::t -> let mod_st = {st with current_player = player_at_cell h} in
      let move = cell_coords h in
      let com = Place move in
      let st' = do' com mod_st in
      krazy_recalc_helper t st'


let krazy_recalc_score st =
  let num_p = string_of_num_p (game_num_plyrs st) in
  let p1_av = string_of_player (p1_avatar st) in
  let lvl = string_of_level (game_level st) in
  let mode = string_of_mode (game_mode st) in
  let info_str = num_p^" "^p1_av^" "^lvl^" "^mode in
  let win_inst_tracker = [] in
  let occupied = cells_occupied (board st) in
  let new_st = init_state info_str in
  krazy_recalc_helper occupied new_st
  (* List.map (fun x -> get_all_win_inst st x) occupied *)

let krazy_disappearing_sqs st c =
  if (c=c) then (
    Hashtbl.replace st.tttBoard c.cell {c with player = None}
  ) else ()

let krazy_cell_swap st coords1 coords2 =
  let orig_b = board st in
  let b = copy orig_b in
  let c1 = get_cell coords1 b in
  let c2 = get_cell coords2 b in
  let c1_player = player_at_cell c1 in
  let c2_player = player_at_cell c2 in
  Hashtbl.replace b coords1 {c1 with player = c2_player};
  Hashtbl.replace b coords2 {c2 with player = c1_player};
  {st with tttBoard = b} |> krazy_recalc_score

let krazy_switch_planes st pl1 pl2 =
  let b = board st in
  let copy_b = copy empty_board in
  Hashtbl.iter (fun (p, r, c) v ->
    if p = pl1 then Hashtbl.replace copy_b (pl2, r, c) {v with cell = (pl2, r, c)}
    else if p = pl2 then Hashtbl.replace copy_b (pl1, r, c) {v with cell = (pl1, r, c)}
    else Hashtbl.add copy_b (p, r, c) v) b;
  Hashtbl.remove copy_b (1, 1, 1);
  {st with tttBoard = copy_b} |> krazy_recalc_score

let krazy_bomb st coords =
  let orig_b = board st in
  let b = copy orig_b in
  let c = get_cell coords b in
  let col_3d = find_vertical_cells c b in (*cell list list*)
  let v_3d_diag = List.flatten (vertical_3d_groups c b) in (*cell list list*)
  let h_3d_diag = List.flatten (horizontal_3d_group c b) in (*cell list list*)
  let plane_2d_inst = List.flatten (three_row_2d_cells c b) in (*cell list list *)
  let instances = col_3d @ v_3d_diag @ h_3d_diag @ plane_2d_inst in
  let inst_list = extract_cell_pos instances in
  let b' = List.iter (fun i ->
      Hashtbl.replace b (Hashtbl.find b i).cell {(Hashtbl.find b i) with player=None}) inst_list in
  {st with tttBoard = b} |> krazy_recalc_score
