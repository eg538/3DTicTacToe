(** This would be our main.ml where we would take care of the REPL
 *feature for the MVC *)
open Types
open Command
open State
open Parse_init
open ANSITerminal

exception Terminated
exception Restart

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "Python"
  | Caml -> "Caml"
  | None -> "None"

(*[play st] is the helper function for play_game ()*)
let rec play st =
  print_endline "Please enter command";
  let com = read_line () in
  let command = parse com in
  let newSt = do' command st in
  (*Remember to check for win*)
  match command with
  | Play str -> print_endline "A game is currently is session. Please quit first.";
    play newSt
  | Score ->
    print_endline ("Score of player 1: "^(string_of_int (p1_score st))^"\n"^"Score of player 2: "^(string_of_int (p2_score st)));
    play newSt
  | Quit -> raise Terminated
  | Restart -> raise Restart
  | Try (pl, x, y) -> failwith "Unimplemented"
  | Place (pl, x, y) ->
      if newSt = st then
        print_endline "Action impossible. Please try a different move."
      else
        print_board newSt;
        print_endline ("Score of player 1: "^(string_of_int (p1_score newSt))^"\n"^"Score of player 2: "^(string_of_int (p2_score newSt)));
      play newSt
  | Hint -> failwith "Unimplemented"
  | Look -> print_board st; play newSt
  | CurrentPlayer ->
    print_endline ("Current player: "^(string_of_player (curr_player st)));
    play newSt
  | Invalid -> print_endline "Action impossible. Please try a different move.";
    play newSt

(*[play_game ()] is the simulation of a 3d tic-tac-toe game. It takes in commands
 * from the user and then progresses the game accordingly*)
let rec play_game () =
try (
  print_endline "Please type play and the name of the file you desire.";
  print_endline "> ";
  let com = read_line () in
  let command = parse com in
  begin
  match command with
  | Play str ->
    let init_st = init_state str in
    print_board init_st;
    begin
    try(
      play init_st
      ) with
    | Terminated -> print_endline "Bye!"
    | Restart -> play_game ()
    | _ -> print_endline "Error"
    end
  | _ -> print_endline "Invalid command. No ongoing game."
  end
) with
| _ -> print_endline "No such game file found"

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to 3D Tic Tac Toe!\n");
  ANSITerminal.(print_string [blue]
                  ("\nHere are the following commands:\n"^
                  "- place x, y, z : makes a move at cell x, y, z, where x represents the plane [0, 1, 2], y represents the row [0, 1, 2] and z represents the column [0, 1, 2]\n"^
                  "- score : displays the scores of the players\n"^
                  "- look : displays the board again\n"^
                  "- quit : quits the game\n"^
                  "- current player : displays the avatar of the current player\n"));
  play_game ()

let () = main ()
