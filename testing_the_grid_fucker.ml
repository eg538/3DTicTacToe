open Grid_2d
open State
type cell = {cell: (int*int*int); taken: bool; player:  player}
type winType3D =
  | WinV of cell list
  | WinH of cell list
  | WinNone

type player = None |  Python |  Caml

module type Plane = sig
  val get_plane : (int*int*int) -> int
  val get_parent_plane: cell -> cell list -> cell list
  val cells_left: cell list -> cell list
  val is_taken : (int*int*int) -> cell list -> bool
  val cell_valid : cell -> bool
  val three_row_2d: cell -> cell list -> bool
  val move_valid: cell -> cell list ->  bool
end



module TopPlane = struct
  let get_plane cell_pos =
    match cell_pos with
    | (pl, _ , _ ) -> pl

  let rec get_parent_plane cell_pos lst_of_cells =
    let pl_match = get_plane cell_pos.cell in
    List.filter (fun x -> (get_plane x.cell) = pl_match ) lst_of_cells

  let cells_left lst =
    List.filter (fun x -> x.taken = false) lst

  let is_taken cell_pos lst =
    List.exists (fun x -> ((x.cell = cell_pos) && (x.taken = false)) ) lst

  let cell_valid cell =
    match cell.cell with
    | (pl, x, y) -> if pl = 1 && x = 1 && y = 1 then false
      else if x < 0 || x >=3 then false
      else if y < 0 || y >=3 then false
      else true

  (* fix !!*)
  let move_valid cell lst =
    (cell_valid cell) && (is_taken cell.cell lst)

  let get_x cell_loc =
    match cell_loc with
    | (_, ex, _) -> ex

  let get_y cell_loc =
    match cell_loc with
    | (_, _, why) -> why

  let three_row_2d cell lst_of_cells =
    let cell_player = cell.player in
    let cell_ex = get_x cell.cell in
    let cell_why = get_y cell.cell in
    let cell_pl = get_plane cell.cell in
    (* three in a column *)
    ((List.mem {cell = (cell_pl, cell_ex, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))

    ||

    (* three in a row *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))

    ||

    (* three in a diagonal starting from leftmost corner *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells )))

    ||

    (* three in a diagonal starting from rightmost corner *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))
end

module MidPlane = struct
  let get_plane cell_pos =
    match cell_pos with
    | (pl, _ , _ ) -> pl

  let rec get_parent_plane cell_pos lst_of_cells =
    let pl_match = get_plane cell_pos.cell in
    List.filter (fun x -> (get_plane x.cell) = pl_match ) lst_of_cells

  let cells_left lst =
    List.filter (fun x -> x.taken = false) lst

  let is_taken cell_pos lst =
    List.exists (fun x -> ((x.cell = cell_pos) && (x.taken = false)) ) lst

  let cell_valid cell =
    match cell.cell with
    | (pl, x, y) -> if pl = 1 && x = 1 && y = 1 then false
      else if x < 0 || x >=3 then false
      else if y < 0 || y >=3 then false
      else true

  (* fix !!*)
  let move_valid cell lst =
    (cell_valid cell) && (is_taken cell.cell lst)

  let get_x cell_loc =
    match cell_loc with
    | (_, ex, _) -> ex

  let get_y cell_loc =
    match cell_loc with
    | (_, _, why) -> why

  let three_row_2d cell lst_of_cells =
    let cell_player = cell.player in
    let cell_ex = get_x cell.cell in
    let cell_why = get_y cell.cell in
    let cell_pl = get_plane cell.cell in
    (* three in a column *)
    ((List.mem {cell = (cell_pl, cell_ex, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))

    ||

    (* three in a row *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))

    ||

    (* three in a diagonal starting from leftmost corner *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells )))

    ||

    (* three in a diagonal starting from rightmost corner *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))
end

module BottomPlane = struct
  let get_plane cell_pos =
    match cell_pos with
    | (pl, _ , _ ) -> pl

  let rec get_parent_plane cell_pos lst_of_cells =
    let pl_match = get_plane cell_pos.cell in
    List.filter (fun x -> (get_plane x.cell) = pl_match ) lst_of_cells

  let cells_left lst =
    List.filter (fun x -> x.taken = false) lst

  let is_taken cell_pos lst =
    List.exists (fun x -> ((x.cell = cell_pos) && (x.taken = false)) ) lst

  let cell_valid cell =
    match cell.cell with
    | (pl, x, y) -> if pl = 1 && x = 1 && y = 1 then false
      else if x < 0 || x >=3 then false
      else if y < 0 || y >=3 then false
      else true

  (* fix !!*)
  let move_valid cell lst =
    (cell_valid cell) && (is_taken cell.cell lst)

  let get_x cell_loc =
    match cell_loc with
    | (_, ex, _) -> ex

  let get_y cell_loc =
    match cell_loc with
    | (_, _, why) -> why

  let three_row_2d cell lst_of_cells =
    let cell_player = cell.player in
    let cell_ex = get_x cell.cell in
    let cell_why = get_y cell.cell in
    let cell_pl = get_plane cell.cell in
    (* three in a column *)
    ((List.mem {cell = (cell_pl, cell_ex, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))

    ||

    (* three in a row *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))

    ||

    (* three in a diagonal starting from leftmost corner *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells )))

    ||

    (* three in a diagonal starting from rightmost corner *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))
end

(* module type Plane = sig
   val get_plane : (int*int*int) -> int
   val get_parent_plane: cell -> cell list -> cell list
   val cells_left: cell list -> cell list
   val is_taken : (int*int*int) -> cell list -> bool
   val cell_valid : cell -> bool
   val three_row_2d: cell -> cell list -> bool
   val move_valid: cell -> cell list ->  bool
   end *)

(* module PlaneTester = struct
   let get_plane c = failwith "Unimplemented"
   let get_parent_plane c cl = failwith "Unimplemented"
   let cells_left cl = failwith "Unimplemented"
   let is_taken c cl = failwith "Unimplemented"
   let cell_valid c = failwith "Unimplemented"
   let three_row_2d c cl = failwith "unimpl"
   let move_valid c cl = failwith "unimpl"
   end

   module MakePlane
   = functor (K : Plane) ->
   struct

    let get_plane cell_pos =
      match cell_pos with
      | (pl, _ , _ ) -> pl

    let rec get_parent_plane cell_pos lst_of_cells =
      let pl_match = get_plane cell_pos.cell in
      List.filter (fun x -> (get_plane x.cell) = pl_match ) lst_of_cells

    let cells_left lst =
      List.filter (fun x -> x.taken = false) lst

    let is_taken cell_pos lst =
      List.exists (fun x -> ((x.cell = cell_pos) && (x.taken = false)) ) lst

    let cell_valid cell =
      match cell.cell with
      | (pl, x, y) -> if pl = 1 && x = 1 && y = 1 then false
        else if x < 0 || x >=3 then false
        else if y < 0 || y >=3 then false
        else true

    (* fix !!*)
    let move_valid cell lst =
      (cell_valid cell) && (is_taken cell.cell lst)

    let get_x cell_loc =
      match cell_loc with
      | (_, ex, _) -> ex

    let get_y cell_loc =
      match cell_loc with
      | (_, _, why) -> why

    let three_row_2d cell lst_of_cells =
      let cell_player = cell.player in
      let cell_ex = get_x cell.cell in
      let cell_why = get_y cell.cell in
      let cell_pl = get_plane cell.cell in
      (* three in a column *)
      ((List.mem {cell = (cell_pl, cell_ex, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
       (List.mem {cell = (cell_pl, cell_ex, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))

      ||

      (* three in a row *)
      ((List.mem {cell = (cell_pl, cell_ex -1, cell_why); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
       (List.mem {cell = (cell_pl, cell_ex + 1, cell_why); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))

      ||

      (* three in a diagonal starting from leftmost corner *)
      ((List.mem {cell = (cell_pl, cell_ex -1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
       (List.mem {cell = (cell_pl, cell_ex + 1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells )))

      ||

      (* three in a diagonal starting from rightmost corner *)
      ((List.mem {cell = (cell_pl, cell_ex -1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)) &&
       (List.mem {cell = (cell_pl, cell_ex + 1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell lst_of_cells)))
   end *)

(*grid_space takes in 3 planes *)
let init =
  (TopPlane, MidPlane, BottomPlane)

let thd x = function
  | _,_,y -> y
  | _ -> failwith "Impossible"

(*let grid_space_construction planes = (*huge ass cell-list*)
  let gs = Grid_2d.get_parent_plane (0,0,0) @ plane2.get_cells in
  let gs' = plane3.get_cells @ gs in (*can't use get_parent_plane here*)
  gs'
*)
(*grid_space = state.cells *)
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
  let cell_1 = (find_vertical_cells c grid_space) |> fst in
  let cell_2 = (find_vertical_cells c grid_space) |> snd in
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
