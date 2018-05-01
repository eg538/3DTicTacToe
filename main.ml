(** This would be our main.ml where we would take care of the REPL
 *feature for the MVC *)
open Command
open State
open Graphics
open Camlimages

exception Terminated
exception Restart

let string_of_player p = match p with
  | Python -> "Python"
  | Caml -> "Caml"
  | None -> "None"

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
        print_board st;
        print_endline ("Score of player 1: "^(string_of_int (p1_score st))^"\n"^"Score of player 2: "^(string_of_int (p2_score st)));
      play newSt
  | Hint -> failwith "Unimplemented"
  | Look -> print_board st; play newSt
  | CurrentPlayer ->
    print_endline ("Current player: "^(string_of_player (curr_player st)));
    play newSt
  | Invalid -> print_endline "Action impossible. Please try a different move.";
    play newSt

let rec play_game () =
try (
  print_endline "Please type play and the name of the file you desire.";
  print_string  "> ";
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
                  "\n\nWelcome to 3D Tic Tac Toe.\n");
  print_int([|[|black|];[|black|]|] |> Array.length);
  Graphics.open_graph " 1000x750";
  Gui.init_welcome();
  play_game ()

let () = main ()
