(*TODO: should be able to call Top, Bottom, Middle of type Grid 2D*)

(* [get_parent_plane cell] gets the cell list plane that the current [cell] is
   in. Maybe the return type would be of type elevation, depending on how we
   hardcode. resolution needed (*FIXME*)
*)

val get_parent_plane: cell -> cell list (*(? or a variant??) *)

(* [win_evaluation] takes in [cell] and the 3 [cell list]s making up the grid
   space and determines whether the [cell] is part of a newly won 3-in-a-row
   instance.
*)

val win_evaluation: cell -> cell list -> cell list -> cell list -> true

(* [cells_occupied] keeps track of the cells in the 3d grid space that have been
   played by a player. The accumulating list of cells would be of type
   [cell list] and you take in the 3d grid space as 3 separate [cell list]s as
   inputs
*)

val cells_occupied: cell list -> cell list -> cell list -> cell list

  (*
(* [three_row_col_3d] makes sure that a given three cells, one in each grid
   plane of type [cell list] respectively, is a valid win for
   a specified player. This specifically checks vertical
   3-in-a-row instances in the 3d grid space.
*)

val three_row_col_3d: cell list -> cell list -> cell list -> bool

(* [three_row_diag_3d] makes sure that a given three cells, one in each grid
   plane of type [cell list] respectively, is a valid win
   for a specified player. This specifically checks diagonal
   3-in-a-row instances in the 3d grid space.
*)

val three_row_diag_3d:
*)

(* [accounted_for cell list] keeps track of the 3-in-a-row wins of either
   X's or O's. It is not dependent on the player's icon in the
   game. The 3-in-a-row wins are kept in a list of triples,
   where each triple corresponds to a location in the grid. If a given cell list
   is already found, then we don't want to count it again.
 *)
  (*added into accounted_for list, list of 3 triples () *)
  val accounted_for: cell list -> bool
