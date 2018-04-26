open Grid_2d
(*TODO: should be able to call Top, Bottom, Middle of type Grid 2D, already defined in grid_2d.ml*)

(* [win_evaluation] takes in [cell] and the 3 [cell list]s making up the grid
   space and determines whether the [cell] is part of a newly won 3-in-a-row
   instance.
*)

val win_evaluation: cell -> cell list -> cell list -> cell list -> bool

(* [cells_occupied] keeps track of the cells in the 3d grid space that have been
   played by a player. The accumulating list of cells would be of type
   [cell list] and you take in the 3d grid space as 3 separate [cell list]s as
   inputs
*)

val cells_occupied: cell list -> cell list -> cell list -> cell list

(* [diag_check cell] makes sure that a given three cells, one in each grid
   plane of type [cell list] respectively, is a valid win
   for a specified player. This specifically checks diagonal
   3-in-a-row instances in the 3d grid space. For both [cell list]s, they are
   lists of occupied cells in plane p/q
*)
val diag_check: cell -> cell list -> cell list -> bool

(* [col_check cell] makes sure that a given three cells, one in each grid
   plane of type [cell list] respectively, is a valid win for
   a specified player. This specifically checks vertical
   3-in-a-row instances in the 3d grid space. For both [cell list]s, they are
   lists of occupied cells in plane p/q
*)
val col_check: cell -> cell list -> cell list -> bool
