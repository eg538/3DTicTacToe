open Grid_2d


let diag_check c plane1 plane2 =


let thd x = function
  | _,_,y -> y
  | _ -> failwith "Impossible"

let col_check c plane1 plane2 cell_player =
  match thd c.cell with
  | 0 ->
    begin
    let cell_1 = {cell = (cell_pl, cell_ex -1, (thd c.cell+1)); taken = true; player = cell_player} in
  let cell_2 = {cell = (cell_pl, cell_ex -1, (thd c.cell+2)); taken = true; player = cell_player}
  match c.cell with
  | x, y, thd c.cell -> if {cell = (cell_pl, cell_ex -1, cell_why -1); taken = true; player = cell_player}
  | _ -> failwith "Impossible"

let win_evaluation c p1 p2 p3 =
  let cases_3d = (diag_check c) || (col_check c) in
  let case_1 = (three_row_2d c p2) || (three_row_2d c p3) in
  let case_2 = (three_row_2d c p1) || (three_row_2d c p3) in
  let case_3 = (three_row_2d c p1) || (three_row_2d c p2) in
  match get_plane c with
  | 0 -> case_1 || cases_3d
  | 1 -> case_2 || cases_3d
  | 2 -> case_3 || cases_3d

let cells_occupied p1 p2 p3 = (*is each plane represented as a cell list *)
  let whole_space = List.fold_left (fun a x -> x::a) [] [p1;p2;p3] in
  List.filter (cell.taken = false) whole_space
