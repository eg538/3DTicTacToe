  type player = None |  Python |  Caml
type cell = {cell: (int*int*int); taken: bool; player:  player}

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
