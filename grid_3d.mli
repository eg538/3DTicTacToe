(* [accounted_for] keeps track of the number of 3-in-a-row wins of either
                   X's or O's. It is not dependent on the player's icon in the
                   game. The 3-in-a-row wins are kept in a list of triples,
                   where each triple corresponds to a location in the grid.
*)

val accounted_for: cell list

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
