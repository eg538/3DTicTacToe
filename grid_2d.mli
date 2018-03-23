type player = None | Python | Caml
type cell = {cell: (int*int*int); taken: bool; player: player}

(** [positions] is a list of the cells that make up each plane. Because each
  * plane is 3 x 3, the length of this list should be nine. The list is ordered
  * by the row element of the cell tuple (as plane will be the same for all 9
  * cells). That is the order for list (regardless of the plane) should be:
  * [(_,row1,col1)  , (_,row1,col2), ..., (_, row2, col1), ..., (_,row3, col1),
  * ... (_, row3, col3)] (entirety of record data has been omitted to illustrate
  * the ordering)
 **)
val positions : cell list


(** [cells_left lst] is a list of the cells that are still free. If no cells are
  * free, then [cells_left lst] returns []. The list is unordered. Used to know
  * when game is over.
 **)
val cells_left : cell list -> cell list

(** [cells_occupied lst] is a list of the cells that are occupied. If none of
  * the cells are occupied, then it returns []. The list is unordered.
 **)
val cells_occupied: cell list -> cell list

(** [three_row_2d input_cell lst] is true if the inputted cell makes a vertical,
  * diagonal, or horizontal line with the same players. It is false otherwise.
  * Requires: lst to be a list of occuppied cells, the cell the player just
  * placed their marker in
  * Example:
  *  - let cell_input = {cell = (0,0,2); taken = true; player = caml} in
  *  - let cell2 = {cell = (0,0,0); taken = true; player = caml} in
  *  - let cell3 = {cell = (0,0,1); taken = true; player - caml} in
  *  - let cell_occupied = [cell2, cell3] in
  *  - [three_row_2d cell_input cell_occupied] returns true
  *  - (it forms a horizontal line in the top plane in the top row)
 **)
val three_row_2d: cell -> cell list -> bool

(** [move_valid cell lst] is true if the user can play the move they want to
  * play. [cell] is the move the player wishes to make. [lst] is the list of
  * all the cells in the plane.
  * The move is INVALID if:
  *   - row < 0 OR row > 2
  *   - col < 0 OR col > 2
  *   - taken is true for that cell in [lst]
 **)
val move_valid: cell -> cell list -> bool

(* [retrieve_same_plane_match cell] returns the three-in-a-row instance in the
   form of a [cell list] including the given cell that was just played. This
   would check only return a three-in-a-row instance on the plane that the
   particular cell is currently on -- the rest of the possible 3d instances are
   checked in grid_3d.ml - cdc222
*)
val retrieve_same_plane_match: cell -> cell list
