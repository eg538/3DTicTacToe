open Graphics
open State

(*Draws the welcome screen, with the title of the game and creators*)
val init_welcome : unit -> unit

(*Draw the menu so the player can choose between single and multiplayer games*)
val init_menu : unit -> string -> unit
(*TODO: Would involve draw canvas and draw string and mouseover functionality*)

(*allows the player to choose between crazy and normal mode*)
val init_mode : unit -> string -> unit

(*allows the player to choose their avatar*)
val init_avatar : unit -> image -> image ->string -> unit

(*draws the board to start the game *)
val draw_board : state -> unit

(*updates the board during the game*)
val update_board : state -> unit

(*draws the outcome screen of the game*)
val draw_outcome : state -> image -> unit
