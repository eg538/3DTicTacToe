(** This would be our main.ml where we would take care of the REPL
 *feature for the MVC *)
open Types
open Command
open State
open Ai
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

let computer_move_st newSt = 
  print_endline "Please wait while computer moves...";
  let comp_move = 
    if game_level newSt = Easy then
      easy_ai_move newSt
    else if game_level newSt = Medium then
      medium_ai_move newSt
    else
      hard_ai_move newSt
  in
    let newSt' = do' comp_move newSt in
    print_board newSt';
    print_endline ("Score of player 1: "^(string_of_int (p1_score newSt'))^
                  "\n"^"Score of player 2: "^(string_of_int (p2_score newSt')));
    newSt'

let rec ended () =
  let grab_GUI = Gui.play_board () in
  let com = fst grab_GUI in
  let command = parse com in
  match command with
  | Quit -> print_endline "You have chosen to quit game"
  | Restart -> raise Restart
  | _ -> ended ()

(*[play st] is the helper function for play_game ()*)
let rec play single st=
  if game_ended st then
    let win_msg_and_stuff = get_result_message st in
    let win_msg = snd win_msg_and_stuff in
    print_endline win_msg;
    print_endline (fst win_msg_and_stuff);
    (Gui.winner_winner_chicken_dinner (fst win_msg_and_stuff));
    ended ()
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
  match command with
  | Play str -> (print_endline "A game is currently is session. Please quit first.";
                 play single newSt)
  | Score ->
    (print_endline ("Score of player 1: "^(string_of_int (p1_score st))^"\n"^"Score of player 2: "^(string_of_int (p2_score st)));
     play single newSt)
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
       play single newSt;)

      (*if newSt = st then
          print_endline "Action impossible. Please try a different move."
        else
          print_board newSt;
          print_endline ("Score of player 1: "^(string_of_int (p1_score newSt))^"\n"^"Score of player 2: "^(string_of_int (p2_score newSt)));
          if newSt <> st && single then
            play single (computer_move_st newSt)
          else
            play single newSt*)

    else
      (  print_board newSt;
      let x = snd test |> fst in
      let y = snd test |> snd in
      Gui.cover_up ();
      print_int x;
      print_int y;
      Gui.responsive_board playerr x y ;
      let player1_score = p1_score newSt in
      let player2_score = p2_score newSt in
      Gui.score player1_score player2_score ;
      print_endline ("Score of player 1: "^(string_of_int (p1_score newSt))^"\n"^"Score of player 2: "^(string_of_int (p2_score newSt)));
    play single newSt))
  | Hint -> (failwith "Unimplemented")
  | Look -> (print_board st; play single newSt)
  | CurrentPlayer ->
    (print_endline ("Current player: "^(string_of_player (curr_player st)));
     play single newSt)
  | Invalid -> (print_endline "Action impossible. Please try a different move.";
                play single newSt)
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
        play (game_num_plyrs init_st <> Multi) init_st
      ) with
    | Terminated -> print_endline "Bye!"
    | Restart -> (print_endline "You have chosen to restart this game";
                  f ())
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
