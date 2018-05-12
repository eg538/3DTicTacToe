open Types
open Grid_3d
 open State 
let krazy_disappearing_sqs st c =
  if (c=c) then (
    Hashtbl.replace st.tttBoard c.cell {c with player = None}
  ) else ()

let krazy_cell_swap st c c1 c2 = failwith "unimplemented"

let rec krazy_recalc_helper cellst st =
  match cellst with
  | [] -> st
  | h::t -> let mod_st = {st with current_player = player_at_cell h} in
      let move = cell_coords h in
      let com = Place move in
      let st' = do' com mod_st in
      krazy_recalc_helper t st'


let krazy_recalc_score st info_str=
  let win_inst_tracker = [] in
  let occupied = cells_occupied (board st) in
  let new_st = init_state info_str in
  krazy_recalc_helper occupied new_st
  (* List.map (fun x -> get_all_win_inst st x) occupied *)

let krazy_switch_planes st pl1 pl2 =
  let b = board st in
  let copy_b = copy empty_board in
  Hashtbl.iter (fun (p, r, c) v ->
    if p = pl1 then Hashtbl.replace copy_b (pl2, r, c) {v with cell = (pl2, r, c)}
    else if p = pl2 then Hashtbl.replace copy_b (pl1, r, c) {v with cell = (pl1, r, c)}
    else Hashtbl.add copy_b (p, r, c) v) b;
  Hashtbl.remove copy_b (1, 1, 1);
  {st with tttBoard = copy_b}

let krazy_bomb st c =
  let b = board st in
  let col_3d = find_vertical_cells c b in (*cell list list*)
  let v_3d_diag = List.flatten (vertical_3d_groups c b) in (*cell list list*)
  let h_3d_diag = List.flatten (horizontal_3d_group c b) in (*cell list list*)
  let plane_2d_inst = List.flatten (three_row_2d_cells c b) in (*cell list list *)
  let instances = col_3d @ v_3d_diag @ h_3d_diag @ plane_2d_inst in
  let inst_list = extract_cell_pos instances in
  let b' = List.iter (fun i ->
      Hashtbl.replace b (Hashtbl.find b i).cell {(Hashtbl.find b i) with player=None}) inst_list in
  st (*tttBoard = b'*)
