open State

module type Plane = sig
  type cell
  val get_plane : (int*int*int) -> int
  val get_parent_plane: cell -> state -> cell list
  val cells_left: cell list -> cell list
  val is_taken : cell -> cell list -> bool
  val cell_valid : cell -> bool
  val three_row_2d: cell -> cell list -> bool
  val move_valid: cell -> cell list -> bool
end

type player = None |  Python |  Caml

type cell = {cell: (int*int*int); player:  player}


module Top: Plane = struct

  type cell

  let get_plane cell_pos =
    match cell_pos with
    | (pl, _ , _ ) -> pl

  let rec get_parent_plane cell_pos st =
    let pl_match = get_plane cell_pos.cell in
    let cell_list = st.cells in
    List.filter (fun x -> get_plane x.cell = pl_match ) cell_list

  let cells_left lst =
   List.filter (fun x -> x.taken = false) lst

  let is_taken cell lst =
    List.exists (fun x -> x.cell = cell && x.taken = false ) lst

  let cell_valid cell =
    match cell.cell with
    | (pl, x, y) -> if pl = 1 && x = 1 && y = 1 then false
      else if x < 0 || x >=3 then false
      else if y < 0 || y >=3 then false
      else true

  let move_valid cell lst =
    (cell_valid cell) && (is_taken cell lst)

  let get_x cell_loc =
    match cell_loc with
    | (_, ex, _) -> ex

  let get_y cell_loc =
    match cell_loc with
    | (_, _, why) -> why

  let three_row_2d cell st =
    let cell_player = cell.player in
    let cell_ex = get_x cell.cell in
    let cell_why = get_y cell.cell in
    let cell_pl = get_plane cell.cell in
    (* three in a column *)
    ((List.mem {cell = (cell_pl, cell_ex, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell st)) &&
     (List.mem {cell = (cell_pl, cell_ex, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell st)))

    ||

    (* three in a row *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why); taken = true; player = cell_player} (get_parent_plane cell st)) &&
    (List.mem {cell = (cell_pl, cell_ex + 1, cell_why); taken = true; player = cell_player} (get_parent_plane cell st)))

    ||

    (* three in a diagonal starting from leftmost corner *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell st)) &&
     (List.mem {cell = (cell_pl, cell_ex + 1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell st )))

    ||

    (* three in a diagonal starting from rightmost corner *)
    ((List.mem {cell = (cell_pl, cell_ex -1, cell_why + 1); taken = true; player = cell_player} (get_parent_plane cell st)) &&
    (List.mem {cell = (cell_pl, cell_ex + 1, cell_why -1); taken = true; player = cell_player} (get_parent_plane cell st)))




end

module Middle: Plane = struct
end

module Bottom: Plane = struct
end
