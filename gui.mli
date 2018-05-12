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
(*val level_choice : unit -> string*)

(* will draw a rectangle around the krazy/normal mode the player chooses and send that information *)
(*val krazy_choice : unit -> string*)

(*Will draw a rectangel around the number of people playing and send that information *)
(*val num_player : unit -> string*)

(* Will bring the player to the play screen when press start *)
(*val start_choice: unit -> unit*)

(* Causes the warning message (stay inside the lines, can't play in (1,1,1), etc ) to disappear *)
val cover_up: unit -> unit

(* returns the command telling the game to place a marker on a certain cell based on where the player
   clicked on the gui and returns the coordinates where the python and caml images should be drawn
*)
val play_board : unit -> string * (int * int)

(* Draws the python or caml picture (determined by the string parameter) at the location
   specified by the two ints
*)
val responsive_board: string -> int -> int->  unit

(* Takes care of the case when two players want to place their marker in the same cell.
   An error message will appear *)
val repeat_cell : int -> int -> unit

(* Draws a rectangle around the image of the current player *)
val highlight_curr_player: string -> unit

    (* D*)

(* Displays the win/lose/draw message *)
val winner_winner_chicken_dinner: string -> unit
