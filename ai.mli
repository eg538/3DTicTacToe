(**
  * [game_tree] is the main engine for the artificial intelligence aspect of the game.
  * This is mainly accessed when the player type "hint", and then the state of the game is recieved
  * or when the game is played in 'single player' mode, and depending on the level ('easy', 'medium', or 'hard')
  * the game tree will choose the sub-optimal to optimal position for the next move on the game board and
  * thus return a new valid state.
  * requires: a valid state
 **)

type valid_node = {board: cell list -> cell list -> cell list move: int*int*int}
type tree = Leaf | Node of valid_node * tree * tree

val game_tree_generate: valid_node -> tree

val available_cells: state -> cell list

(* cell list is generated from available cells *)
val children: valid_node -> cell list -> valid_node list

(*TBD*)
val heuristic_fxn: valid_node -> int

val alpha_beta_pruned_tree: tree -> tree

(*
*)
val player_hint: tree -> cell
(**)
val computer_turn: tree -> cell
