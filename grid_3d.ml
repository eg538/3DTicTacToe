open grid_2d

(*TODO: does everyone agree that we need to "hardcode" all the positions for every level
  in this .ml file. So far, I've "grouped" the positions as a variant.
*)

type elevation = Top | Middle | Bottom

let win_list = ref 0

let get_parent_plane c = (*assumes that the last coordinate determines the elevation *)
  let thd x,y,z = z in
  match (thd c) with
  | 0 -> Bottom
  | 1 -> Middle
  | _ -> Top

let accounted_for instance =
  if (List.mem (instance) win_list) then false
  else (
    win_list := instance::win_list;
    true
  )

  (*WIP *)
let win_evaluation c p1 p2 p3 =
  let plane = get_parent_plane c in
  if (three_row_2d c plane) then (*check with karen...*)
    let three_in_row = (retrieve_same_plane_match c)(*TODO: implement retrieve_match in grid2d; returns a cell-list *)
    (accounted_for three_in_row)
  else
    false

let cells_occupied p1 p2 p3 = (*is each plane represented as a cell list *)
  let whole_space = p1 @ p2 @ p3 in
  List.filter (cell.taken = false) whole_space
