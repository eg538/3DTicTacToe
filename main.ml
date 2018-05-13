(** This would be our main.ml where we would take care of the REPL
 *feature for the MVC *)
open Types
open Command
open State
open Ai
open Parse_init
open ANSITerminal
open Gui
open Graphics

exception Terminated
exception Restart

let fst' (y,_,_) = y

let snd' (_,y,_) = y

let thd (_,_,y) = y

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "python"
  | Caml -> "caml"
  | None -> "none"

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
  let input = Gui.which_command () in
  let commend = fst' input in
  let x = snd' input in
  let y = thd input in
  let grab_GUI = Gui.play_board commend x y in
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
  let playerr = string_of_player (State.curr_player st )
  in print_endline playerr;
  Gui.highlight_curr_player playerr;
  let hint_num = num_hints st in
  let try_num = num_tries st in
  Gui.num_try_hint hint_num 836 585;
  Gui.num_try_hint try_num 171 585;
  let player1_score = p1_score st in
  let player2_score = p2_score st in
  Gui.score player1_score player2_score ;
  Gui.rect_drawn_gray 149 627 69 44;
  Gui.rect_drawn_gray 800 625 93 40;
  Gui.responsive_board playerr 0 700;

  let input = Gui.which_command () in
  let commend = fst' input in
  let x = snd' input in
  print_endline "x is";
  print_int x;
  let y = thd input in
  print_endline "y is";
  print_int y;
  let test = Gui.play_board commend x y in
  let com = fst test in
  print_endline commend;
  print_endline com;
  let st_modified = (  if playerr = "python" then  {st with p1_num_tries = st.p1_num_tries - 1 }
  else  {st with p2_num_tries = st.p2_num_tries - 1}) in
  if com = "try 1,1,1" then play single st_modified else
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
  | Try (pl, x, y) ->(
      if newSt = st then
      (let ex = snd test |> fst in
      let why = snd test |> snd in
      Gui.repeat_cell ex why;
      print_endline "Action impossible. Please try a different move.";
      play single newSt;)
      else
        (
          print_board newSt;
          let x = snd test |> fst in
          let y = snd test |> snd in
          (* let tmp = playerr ^ "_try" in *)
          print_endline "here";
          Gui.cover_up ();
          print_int x;
          print_int y;
          Gui.tried playerr x y;
          (* Gui.responsive_board tmp x y; *)

          play single newSt))
  | Place (pl, x, y) ->
    (if newSt = st then
       (let ex = snd test |> fst in
        let why = snd test |> snd in
        Gui.repeat_cell ex why;
        print_endline "Action impossible. Please try a different move.";
        play single newSt;)

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
          let hint_num = num_hints newSt in
          let try_num = num_tries newSt in
          Gui.num_try_hint try_num 836 587;
          Gui.num_try_hint hint_num 171 593;

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
