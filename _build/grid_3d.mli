type cell
type player = None |  Python |  Caml
(*WinType3D is a variant type used to distinguish the different types of
  "diagonal" wins that could occur in the 3x3x3 space.
*)
type winType3D =
| WinV of cell list
| WinH of cell list
| WinNone

(*[fst' c] returns the first entry of the triple [c]*)
val fst' : (int*int*int) -> int

(*[snd' c] returns the second entry of the triple [c]*)
val snd' : (int*int*int) -> int

(*[thd c] returns the third entry of the triple [c]*)
val thd : (int*int*int) -> int

(*[horizontal_3d_group c grid_space] finds the particular x-plane slice of the
  [grid_space] that contains the potential diagonal three-in-a-row instances
  that [c] could be a part of.
*)
val horizontal_3d_group: cell -> cell list -> cell list

(*[vertical_3d_group c grid_space] finds the particular y-plane slice of the
  [grid_space] that contains the potential diagonal three-in-a-row instances
  that [c] could be a part of.
*)
val vertical_3d_groups: cell -> cell list -> cell list

(*[diag_check c grid_space] assesses whether the potential three-in-a-row
  instances that include [c] are actually newly formed three-in-a-row instances.
  Returns: a winType3D tuple that indicates the type of newly formed three-in-a-
  row instances if any: (either from the x-plane or from the y-plane).
*)
val diag_check: cell -> cell list -> (winType3D * winType3D)

(*[find_vertical_cells c grid_space] finds the remaining cells that are also
  found in [c]'s particular column in [grid_space].
*)
val find_vertical_cells: cell -> cell list -> cell list

(*[col_check c grid_space] checks whether the other cells in [c]'s column in
  [grid_space] are both taken.
*)
val col_check: cell -> cell list -> bool

(* [win_evaluation c p1 p2 p3] determines whether new three-in-a-row instances
   were found overall after putting a move on cell [c].
*)

val win_evaluation: cell -> cell list -> cell list -> cell list -> bool

(* [cells_occupied p1 p2 p3] keeps track of the cells in the 3d grid space that
   have been played by a player. The accumulating list of cells would be of type
   [cell list] and you take in the 3d grid space as 3 separate [cell list]s as
   inputs
*)

val cells_occupied: cell list -> cell list -> cell list -> cell list

(*[get_the_win c current_player p1 p2 p3] returns the cells that are of a newly
  found three-in-a-row instance including [c]
*)

val get_the_win: cell -> player -> cell list -> cell list -> cell list -> cell list
