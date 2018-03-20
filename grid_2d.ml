type cell = {cell: (int*int*int); taken: bool; player: int}

module type Plane = sig
  val positions: cell list
  val cells_left: cell list -> cell list
  val three_row_2d: cell -> cell -> cell -> bool
  val move_valid: cell -> cell list -> bool
end

module Top: Plane = struct
  let positions =
    let tmp_cell = {cell = (0,0,0); row }

  let cells_left lst = failwith "Unimplemented"

  let three_row_2d cell1 cell2 cell3 = failwith "Unimplemented"

  let move_valid cell lst = failwith "Unimplemented"

end
