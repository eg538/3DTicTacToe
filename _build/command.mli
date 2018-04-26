(* [command] represents a command input by a player. *)
type command = 
  | Play 
  | Score 
  | Quit 
  | Restart
  | Try of (int * int * int) 
  | Place of (int * int * int)
  | Hint 
  | Look
  | Turns
  | Start
 (* [parse str] is the command that represents player input [str].
  * requires: [str] is one of the commands forms. *)
val parse: string -> command
