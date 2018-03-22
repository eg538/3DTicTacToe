module type Plane = sig
  type cell
  val positions: cell list
  val cells_left: cell list -> cell list
  val three_row_2d: cell -> cell list -> bool
  val move_valid: cell -> cell list -> bool
end

type player = None | Some Python | Some Caml

type cell = {cell: (int*int*int); taken: bool; player: option player}


module Top: Plane = struct
  include State

  let positions = failwith "Unimplemented"

  let cells_left lst =
    match lst with
    | [] -> []
    | 

  let three_row_2d cell1 lst = failwith "Unimplemented"

  let move_valid cell lst = failwith "Unimplemented"

end
