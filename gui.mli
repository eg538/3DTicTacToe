open Graphics
open State
open Camlimages
open Types

exception Restart
exception Quit

type choices

(*Draws the welcome screen, with the title of the game and creators*)
val init_welcome : unit -> string

(* Draws a gray rectangle around the button the player presses.
   [rect_drawn_gray x y width height] draws a gray rectangle with lower left
   corner at position (x,y) with width [width] and height [height]
 *)
val rect_drawn_gray : int -> int -> int -> int -> unit

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
   [play_board command] will return the concatenated form of "[command] cell" and a tuple of
   coordinates to determine where the picture should be placed. Example of commands include:
   "place", "try"
*)
val play_board : string -> int -> int -> string * (int * int)

(* Draws the python or caml picture (determined by the string parameter) at the location
   specified by the two ints
*)
val responsive_board: string -> int -> int->  unit

(* Takes care of the case when two players want to place their marker in the same cell.
   An error message will appear *)
val repeat_cell : int -> int -> unit

(* Draws a rectangle around the image of the current player *)
val highlight_curr_player: string -> unit

(* Draws the current score for each player. The first int is player1's score and
   the second int is player 2's score
*)
val score : int -> int -> unit

(* Displays the win/lose/draw message *)
val winner_winner_chicken_dinner: string -> unit

(* Displays the number of tries left. int is the number of tries left *)
val num_try_hint : int -> int -> int -> unit

val cover_try : string -> int -> int -> unit


val try_responsive_board: string -> int -> int-> (int * int * int)-> (bool* int * int)


(* Returns which a triple where the first element is the string version
   of command depending on whether or not the user has pressed the try button,
   and the second and third element are the x and y coordinates, respectively of
   where the user pressed
*)
val which_command : unit -> string*int*int

(* Draws a line through the most recent three in a rows *)
val draw_three_row : (int*int*int)  list ->  unit

val cell_coords_to_x_y: (int*int*int) -> (int*int)


val get_img:string -> image

(* Draws a message asking the user to please wait as the computer calculates
   the best move *)
val draw_wait_mgs : unit -> unit

(* [draw_act_two playerr p1_score p2_score hint_num num_tries recent_wins lst] draws the
   the playing board  with all the necessary information
*)
val draw_act_two : string -> int -> int -> int -> int -> (int*int*int) list list -> unit

(* [bomb_animation x y] takes in the x and y position of the icon of the clicked
   cell and will call a helper function (bomb_boom) and will animate a bomb
   booming
*)
val bomb_animation : unit -> unit

(* [krazy_ocur_animation x y] flashes a message telling the user that something
   crazy happened at location x y
*)
val krazy_ocur_animation : unit -> unit
