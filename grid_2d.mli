type player = None | Python | Caml
type cell = {cell: (int*int*int); taken: bool; player: player}


(** [get_plane] is an int tht returns the plane of a given cell. It is a helper
  * function for get_parnet_plane
 **)
val get_plane : int*int*int -> int

(** [get_parent_plane] is a list of the cells that make up a specific [cell]'s plane. Because each
  * plane is 3 x 3, the length of this list should be nine. The list is ordered
  * by the row element of the cell tuple (as plane will be the same for all 9
  * cells). That is the order for list (regardless of the plane) should be:
  * [(_,row1,col1)  , (_,row1,col2), ..., (_, row2, col1), ..., (_,row3, col1),
  * ... (_, row3, col3)] (entirety of record data has been omitted to illustrate
  * the ordering)
  * requires: a valid cell and a valid state
 **)
val get_parent_plane : cell -> state -> cell list

(** [cells_left lst] is a list of the cells that are still free. If no cells are
  * free, then [cells_left lst] returns []. The list is unordered. Used to know
  * when game is over.
 **)
val cells_left : cell list -> cell list

(** [is_taken cell lst] is a bool that is true if the given cell is currently
  * occuppied and false otherwise. It is a helper function for move_valid
 **)
val is_taken : cell -> cell list -> bool

(** [cell_valid cell] is a bool that is true if the given cell is a valid cell. It is a helper function for move_valid**)
val cell_valid : cell -> bool


(** [move_valid cell lst] is true if the user can play the move they want to
  * play. [cell] is the move the player wishes to make. [lst] is the list of
  * all the cells in the plane.
  * The move is INVALID if:
  *   - row < 0 OR row > 2
  *   - col < 0 OR col > 2
  *   - taken is true for that cell in [lst]
 **)
val move_valid: cell -> cell list -> bool

(** [three_row_2d input_cell state] is true if the inputted cell makes a vertical,
  * diagonal, or horizontal line with the same players. It is false otherwise.
  * Requires: current state the game is in
  * Example:
  *  - let cell_input = {cell = (0,0,2); taken = true; player = caml} in
  *  - let cell2 = {cell = (0,0,0); taken = true; player = caml} in
  *  - let cell3 = {cell = (0,0,1); taken = true; player - caml} in
  *  - let cell_occupied = [cell2, cell3] in
  *  - [three_row_2d cell_input cell_occupied] returns true
  *  - (it forms a horizontal line in the top plane in the top row)
 **)
val three_row_2d: cell -> state -> bool
