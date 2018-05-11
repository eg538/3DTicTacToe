(** This would be our main.ml where we would take care of the REPL
 *feature for the MVC *)
open Types
open Command
open State
open Parse_init
open ANSITerminal
open Gui

exception Terminated
exception Restart

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "Python"
  | Caml -> "Caml"
  | None -> "None"

(*[play st] is the helper function for play_game ()*)
let rec play st=
  print_endline "in play now";
  print_endline "Please enter command";
  let com = read_line () in
  let command = parse com in
  let newSt = do' command st in
  if game_ended newSt then
    print_endline (get_result_message newSt)
  else
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

let rec play_game str f =
try (
  print_endline str;
  let command = parse str in
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
    | Restart -> print_endline "You have chosen to restart this game";
        f ()
    | _ -> print_endline "Error"
    end
  | _ -> print_endline "Invalid command. No ongoing game."
  end
) with
| _ -> print_endline "Invalid"

let rec main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to 3D Tic Tac Toe.\n");
  (* print_int([|[|black|];[|black|]|] |> Array.length); *)
  Graphics.open_graph " 1000x750";
  Graphics.set_window_title "3D Tic-Tac-Toe";
  let str = Gui.init_welcome() in
  (* Gui.play_test_two str; *)
  play_game str main;
  (* play_game () *)
  (* print_endline "abvoe gui.play_board()"; *)
  (* Gui.play_board() *)
  (print_endline "help";)

let () = main ()
