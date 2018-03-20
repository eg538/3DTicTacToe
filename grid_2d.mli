type cell

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
  * free, then [cells_left lst] returns []. The list is unordered.
 **)
val cells_left : cell list -> cell list

(** [three_row_2d cell1 cell2 cell3] is true if the three cells have the same
  * marker and form either a vertical line, a horizontal line, or a diagonal
  * line. It is false otherwise. The cells do not have to be in order.
  * [three_row_2d] will return true so long as a continuous line is formed from
  * the three cells.
  * Requires: three valid cells in the same plane.
  * Example:
  *  - let cell1 = {cell = (0,0,2); taken = true; player = caml} in
  *  - let cell2 = {cell = (0,0,0); taken = true; player = caml} in
  *  - let cell3 = {cell = (0,0,1); taken = true; player - caml} in
  *  - [three_row_2d cell1 cell2 cell3] returns true
  *  - (it forms a horizontal line in the top plane in the top row)
 **)
val three_row_2d: cell -> cell -> cell -> bool

(** [move_valid cell lst] is true if the user can play the move they want to
  * play. [cell] is the move the player wishes to make. [lst] is the list of
  * all the cells in the plane.
  * The move is INVALID if:
  *   - row < 0 OR row > 2
  *   - col < 0 OR col > 2
  *   - taken is true for that cell in [lst]
 **)
val move_valid: cell -> cell list -> bool
