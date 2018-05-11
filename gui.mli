open Graphics
open State
open Camlimages
open Types

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

(* Handles the actual game play
   the first int is pos_x, second is pos_y, a string command (e.g: place x,y,z) will be produced and passed into Command.parse and
   the resulting command will be passed into do' which will return a state. In the midst of this, drawings will
   be made
*)
(* val play_heavyduty: int -> int -> state -> state *)

(* string is the starting string to produce the game. unit-> unit is main.
   the last unit is what drawing the gui returns*)
(* val play_board: string -> (unit-> unit)  -> unit *)

(* val play_test_two: string -> unit *)
