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


(* Takes in a unit and will return a string command similar to what the user was
   expected to write in when playing on the terminal. Will pass this string into play.
   Hopefully will be able to then just use play to play the game. *)
val play_board : state -> string

(* val play_heavyduty: int -> int -> state -> state *)

(* string is the starting string to produce the game. unit-> unit is main.
   the last unit is what drawing the gui returns*)
(* val play_board: string -> (unit-> unit)  -> unit *)

(* val play_test_two: string -> unit *)
