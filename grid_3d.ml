open Grid_2d


let diag_check c = failwith "Unimplemented"
let col_check c = failwith "Unimplemented"
  (*WIP *)
let win_evaluation c p1 p2 p3 =
  (three_row_2d c plane) || (diag_check c) || (col_check c)

let cells_occupied p1 p2 p3 = (*is each plane represented as a cell list *)
  let whole_space = List.fold_left (fun a x -> x::a) [] [p1;p2;p3] in
  List.filter (cell.taken = false) whole_space
