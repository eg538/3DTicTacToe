(** Work in progress: 
 * will be edited once we are done with the ml files for grid 3d and 2d. 
*)

(**
  * [game_tree] is the main engine for the artificial intelligence aspect of the game. 
  * This is mainly accessed when the player type "hint", and then the state of the game is recieved 
  * or when the game is played in 'single player' mode, and depending on the level ('easy', 'medium', or 'hard')
  * the game tree will choose the sub-optimal to optimal position for the next move on the game board and 
  * thus return a new valid state.
  * requires: a valid state  
 **)
val game_tree: state -> state 
