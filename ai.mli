(**
  * ai is the main engine for the artificial intelligence aspect of the game.
  * This is mainly accessed when the player type "hint", and when the state of the game is recieved
  * or when the game is played in 'single player' mode, and depending on the level ('easy', 'medium', or 'hard')
  * the game tree will choose the sub-optimal to optimal position for the next move on the game board and
  * thus return a new valid state. This will be implemented using Min and Max search and alpha-beta pruning
 **)

(*[valid_node] is a node in the game search tree. It contains the move that
 * generated the board that is also stored at this node*)
type valid_node = {board: cell list -> cell list -> cell list; move: int*int*int}

(*[tree] is the game search tree. The nodes are of type valid_node. See comments
 * for valid_node for more details*)
type tree = Leaf | Node of valid_node * tree * tree

(*[game_tree_generate nd] is the game search tree generated with nd as the head
 * node of the tree*)
val game_tree_generate: valid_node -> tree

(*[available_cells st] is the list of cells that are vacant in the 3D game board
 * that st contains*)
val available_cells: state -> cell list

(*[children nd clist] is list of nodes that are generated as a result of applying
 * the possible actions specified by clist to the board contained in nd*)
val children: valid_node -> cell list -> valid_node list

(*TBD*)
val heuristic_fxn: valid_node -> int

val alpha_beta_pruned_tree: tree -> tree

(*
*)
val player_hint: tree -> cell
(**)
val computer_turn: tree -> cell
