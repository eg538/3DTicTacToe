open Graphics
open State
open Camlimages

type choices

(*Draws the welcome screen, with the title of the game and creators*)
val init_welcome : unit -> string

(* Draws a magenta rectangle around the button the player presses*)
val rect_drawn : int -> int -> int -> int -> unit

(* Will draw a rectangle around the level the player chooses and send that information *)
val level_choice : unit -> string

(* will draw a rectangle around the krazy/normal mode the player chooses and send that information *)
val krazy_choice : unit -> string

(*Will draw a rectangel around the number of people playing and send that information *)
val num_player : unit -> string

(* Will bring the player to the play screen when press start *)
val start_choice: unit -> unit
