open Grid_2d
open State

type WinType3D =
  | WinV of cell list
  | WinH of cell list
  | WinNone

(*grid_space takes in 3 planes *)
let plane1 = MakePlane(Plane) in
let plane2 = MakePlane(Plane) in
let plane3 = MakePlane(Plane) in


let grid_space_construction plane1 plane2 plane3 = (*plane p1 -> list of cells *)
  let gs = plane1.get_parent_plane @ plane2.get_parent_plane in
  let gs' = plane3.get_parent_plane @ gs in
  gs'

let thd x = function
  | _,_,y -> y
  | _ -> failwith "Impossible"

let horizontal_3d_group c grid_space =
  match c.cell with
  | 0,y,z when (y=z) -> List.filter (fun a -> (a.cell |> snd)=(a.cell |> thd)) grid_space
  | 0,_,_ -> List.filter
               (fun a -> a.cell <> (0,0,1) && a.cell <> (0,1,0) && a.cell <> (0,1,2) && a.cell <> (0.2,1)) grid_space
  | 1,y,z when (y=z) -> List.filter (fun a -> (a.cell |> snd)=(a.cell |> thd)) grid_space
  | 1,_,_ -> List.filter
               (fun a -> a.cell <> (1,0,1) && a.cell <> (1,1,0) && a.cell <> (1,1,2) && a.cell <> (1,2,1)) grid_space
  | 2,y,z when (y=z) -> List.filter (fun a -> (a.cell |> snd)=(a.cell |> thd)) grid_space
  | _ -> List.filter
           (fun a -> a.cell <> (2,0,1) && a.cell <> (2,1,0) && a.cell <> (2,1,2) && a.cell <> (2,2,1)) grid_space

let vertical_3d_groups c grid_space =
  match c.cell with
  | x,0,z when (x=z) -> List.filter (fun a -> (a.cell |> fst)=(a.cell |> thd)) grid_space
  | 0,_,_ -> List.filter
               (fun a -> a.cell <> (0,0,1) && a.cell <> (1,0,0) && a.cell <> (2,0,1) && a.cell <> (1,0,2)) grid_space
  | x,1,z when (x=z) -> List.filter (fun a -> (a.cell |> fst)=(a.cell |> thd)) grid_space
  | 1,_,_ -> List.filter
               (fun a -> a.cell <> (0,0,1) && a.cell <> (1,1,0) && a.cell <> (1,1,2) && a.cell <> (2,1,1)) grid_space
  | x,2,z when (x=z) -> List.filter (fun a -> (a.cell |> fst)=(a.cell |> thd)) grid_space
  | _ -> List.filter
           (fun a -> a.cell <> (0,2,1) && a.cell <> (1,2,0) && a.cell <> (2,2,1) && a.cell <> (1,2,2)) grid_space

let diag_check c grid_space =
  let diag_h = horizontal_3d_group in
  let diag_v = vertical_3d_group in
  let verdict_h = (List.for_all (fun x -> x.taken = true ) diag_h) in
  let verdict_v = (List.for_all (fun x -> x.taken = true) diag_v) in
  match (verdict_h, verdict_v) with
  | true, true -> WinH diag_h, WinV diag_v
  | true, false -> WinH diag_h, WinNone
  | false, true -> WinNone, WinV diag_v
  | false, false -> WinNone, WinNone

let find_vertical_cells c grid_space =
  let f = c.cell |> fst in
  let s = c.cell |> snd in
  List.filter (fun i -> (i.cell |> fst)=f && (i.cell |> snd)=s) grid_space

let col_check c grid_space =
  let cell_1 = (find_vertical_cells c grid_space |> fst in
  let cell_2 = find_vertical_cells c grid_space |> snd in
  (cell_1.taken && (cell_2.taken))

let win_evaluation c p1 p2 p3 =
  let grid_space = p1@p2@p3 in (*3 grids, 3 cell lists *)
  let diag_check_truth = ((diag_check c grid_space |> fst) = WinNone) && ((diag_check c grid_space |> fst) = WinNone) in
  let cases_3d = (diag_check_truth) || (col_check c grid_space) in
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

let get_the_win c current_player p1 p2 p3=
  if (win_evaluation c p1 p2 p3) then
    match (diag_check_truth) with
    | true  -> diag_check
    | _ -> find_vertical_cells c grid_space
  else []
