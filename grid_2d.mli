module type Plane = sig
  type player
  type cell

  (** [get_plane] is an int tht returns the plane of a given cell. It is a helper
    * function for get_parnet_plane
   **)
  val get_plane : (int*int*int) -> int

  (** [get_parent_plane cell lst_of_cells] is a list of the cells that make up a specific [cell]'s plane. Because each
    * plane is 3 x 3, the length of this list should be nine.
    * requires: a valid cell and [lst_of_cells] which is state.cells (a list of all the cells of the board)
   **)
  val get_parent_plane: cell -> cell list -> cell list

  (** [cells_left lst] is a list of the cells that are still free. If no cells are
    * free, then [cells_left lst] returns []. The list is unordered. Used to know
    * when game is over.
   **)
  val cells_left: cell list -> cell list

  (** [is_taken cell lst] is a bool that is true if the given cell is currently
    * occuppied and false otherwise. It is a helper function for move_valid
   **)
  val is_taken : (int*int*int) -> cell list -> bool

  (** [cell_valid cell] is a bool that is true if the given cell is a valid cell.
    * It is a helper function for move_valid
   **)
  val cell_valid : cell -> bool

  (** [three_row_2d input_cell lst_of_cells] is true if the inputted cell makes a vertical,
    * diagonal, or horizontal line with the same players. It is false otherwise.
    * Requires: current state the game is in
    * Example:
    *  - let cell_input = {cell = (0,0,2); taken = true; player = caml} in
    *  - let cell2 = {cell = (0,0,0); taken = true; player = caml} in
    *  - let cell3 = {cell = (0,0,1); taken = true; player - caml} in
    *  - let cell_occupied = [cell2, cell3] in
    *  - [three_row_2d cell_input cell_occupied] returns true
    *  - (it forms a horizontal line in the top plane in the top row)
    * requires: [lst_of_cells] which is state.cells (a list of the cells in the gird)
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
  val move_valid: cell -> cell list ->  bool
end
