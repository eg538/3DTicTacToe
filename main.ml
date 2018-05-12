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
exception GameEnd

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "Python"
  | Caml -> "Caml"
  | None -> "None"

              (* if game_ended newSt then
                (
                  let win_msg = get_result_message newSt in
                  print_endline win_msg;
                  play newSt
                )
                (*raise GameEnd*)
              else *)

(*[play st] is the helper function for play_game ()*)
let rec play st=
  if game_ended st then
    (
      let win_msg = get_result_message st in
      print_endline win_msg
    )
  else
  (print_endline "Please enter command";
  let playerr = match (State.curr_player st ) with
    | Caml -> "caml"
    | Python -> "python"
    | None -> "none"
  in print_endline playerr;
  Gui.highlight_curr_player playerr;
  let test = Gui.play_board () in
  let com = fst test in
  let command = parse com in
  let newSt = do' command st in
    (*Remember to check for win*)
  print_endline "am i here?" ;
  match command with
  | Play str -> (print_endline "A game is currently is session. Please quit first.";
                 play newSt)
  | Score ->
    (print_endline ("Score of player 1: "^(string_of_int (p1_score st))^"\n"^"Score of player 2: "^(string_of_int (p2_score st)));
     play newSt)
  | Quit -> (print_endline "yo what's up in this hole";exit 0)
  | Restart -> (raise Restart)
  | Try (pl, x, y) -> (failwith "Unimplemented")
  | Place (pl, x, y) ->
    (if newSt = st then
      (let ex = snd test |> fst in
      let why = snd test |> snd in
      print_int ex;
      print_int why;
      Gui.repeat_cell ex why;
      print_endline "Action impossible. Please try a different move.";
       play newSt;)
    else
      (  print_board newSt;
    let x = snd test |> fst in
    let y = snd test |> snd in
    Gui.cover_up ();
    print_int x;
    print_int y;
      Gui.responsive_board playerr x y ;
      print_endline ("Score of player 1: "^(string_of_int (p1_score newSt))^"\n"^"Score of player 2: "^(string_of_int (p2_score newSt)));
    play newSt))
  | Hint -> (failwith "Unimplemented")
  | Look -> (print_board st; play newSt)
  | CurrentPlayer ->
    (print_endline ("Current player: "^(string_of_player (curr_player st)));
     play newSt)
  | Invalid -> (print_endline "Action impossible. Please try a different move.";
                play newSt)
  )

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
    | Restart -> (print_endline "You have chosen to restart this game";
                  f ())
    | GameEnd -> print_endline "Game End"
    | _ -> print_endline "Error"
    end
  | _ -> print_endline "Invalid command. No ongoing game."
  end
) with
| _ -> (print_endline "Invalid")

let rec main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to 3D Tic Tac Toe.\n");
  Graphics.open_graph " 1000x750";
  Graphics.set_window_title "3D Tic-Tac-Toe";
  let str = Gui.init_welcome() in
  play_game str main;
  (print_endline "help";)

let () = main ()
