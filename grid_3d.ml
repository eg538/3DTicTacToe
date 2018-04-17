open Grid_2d

let find_diagonal_cells c grid_space =
  match c.cell with
  | 1,y,_ when (y <> 1) -> List.filter (fun i -> (i |> fst)=1) grid_space
  | x,1,_ when (x <> 1) -> List.filter (fun j -> (j |> snd)=1) grid_space
  | x,y,_ when (x = 1) && (y = 1) ->
    begin
      let set_1 = List.filter (fun a -> (a |> fst)=1) grid_space in
      let set_2 = List.filter (fun b -> (b |> snd)=1) grid_space in
      let set_3 = (*diagonals*)

    end (*corners*)
  | 1,1,_ ->
let diag_check c grid_space =


let thd x = function
  | _,_,y -> y
  | _ -> failwith "Impossible"

let find_vertical_cells c grid_space =
  let f = c.cell |> fst in
  let s = c.cell |> snd in
  List.filter (fun i -> (i.cell |> fst)=f && (i.cell |> snd)=s) grid_space

let col_check c grid_space =
  let cell_1 = find_vertical_cells c grid_space |> fst in
  let cell_2 = find_vertical_cells c grid_space |> snd in
  (cell_1.taken && (cell_2.taken))

let win_evaluation c p1 p2 p3 =
  let grid_space = List.flatten [p1;p2;p3] in
  let cases_3d = (diag_check c) || (col_check c grid_space) in
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
