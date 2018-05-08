open Graphics
open State
open Camlimages

(*Draws the welcome screen, with the title of the game and creators*)
val init_welcome : unit -> unit

(* Draws a magenta rectangle around the button the player presses*)
val rect_drawn : int -> int -> int -> int -> unit

(* Will draw a rectangle around the level the player chooses and send that information *)
val level_choice : unit -> unit

(* will draw a rectangle around the krazy/normal mode the player chooses and send that information *)
(* val krazy_choice : unit -> unit

(*Will draw a rectangel around the number of people playing and send that information *)
val num_player : unit -> unit

(* Will bring the player to the play screen when press start *)
val start_choice: unit -> unit *)

(*Draw the menu so the player can choose between single and multiplayer games
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
*)
