open Types
open Grid_3d
(* open State *)
let krazy_disappearing_sqs st c =
  if (c=c) then (
    Hashtbl.replace st.tttBoard c.cell {c with player = None}
  ) else ()

let krazy_cell_swap st c c1 c2 = failwith "unimplemented"

let krazy_recalc_score st b =
  let win_inst_tracker = [] in
  let occupied = cells_occupied b in
  List.map (fun x -> get_all_win_inst st x) occupied
  (*traverse through 2d *)
  (*traverse through 3d*)

let krazy_mode_bomb st c =
  let b = st.tttBoard in
  let col_3d = find_vertical_cells c b in (*cell list list*)
  let v_3d_diag = List.flatten (vertical_3d_groups c b) in (*cell list list*)
  let h_3d_diag = List.flatten (horizontal_3d_group c b) in (*cell list list*)
  let plane_2d_inst = List.flatten (three_row_2d_cells c b) in (*cell list list *)
  let instances = col_3d @ v_3d_diag @ h_3d_diag @ plane_2d_inst in
  let inst_list = extract_cell_pos instances in
  let b' = List.iter (fun i ->
      Hashtbl.replace b (Hashtbl.find b i).cell {(Hashtbl.find b i) with player=None}) inst_list in
  st (*tttBoard = b'*)
