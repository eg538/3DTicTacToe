open State

module type Plane = sig
  type cell
  val get_parent_plane: cell -> cell list
  val cells_left: cell list -> cell list
  val three_row_2d: cell -> cell list -> bool
  val move_valid: cell -> cell list -> bool
  val retrieve_same_plane_match : cell -> cell list
end

type player = None |  Python |  Caml

type cell = {cell: (int*int*int); taken: bool; player:  player}


module Top: Plane = struct

  type cell
  let get_parent_plane = failwith "Unimplemented"

  let cells_left lst = failwith "Unimplemented"

  let three_row_2d cell1 lst = failwith "Unimplemented"

  let move_valid cell lst = failwith "Unimplemented"

  let retrieve_same_plane_match = failwith "Unimplemented"


end

module Middle: Plane = struct
end

module Bottom: Plane = struct
end
