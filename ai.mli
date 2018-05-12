open Types
(**
  * ai is the main engine for the artificial intelligence aspect of the game.
  * This is mainly accessed when the player type "hint", and when the state of the game is recieved
  * or when the game is played in 'single player' mode, and depending on the level ('easy', 'medium', or 'hard')
  * the game tree will choose the sub-optimal to optimal position for the next move on the game board and
  * thus return a new valid state. This will be implemented using Min and Max search and alpha-beta pruning
 **)

val easy_ai_move: state -> command

val medium_ai_move: state -> command

val hard_ai_move: state -> command
