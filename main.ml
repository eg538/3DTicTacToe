(** This would be our main.ml where we would take care of the REPL
 *feature for the MVC *)
open Types
open Command
open State
open Krazy
open Ai
open Parse_init
open ANSITerminal
open Gui
open Graphics

let fst' (y,_,_) = y

let snd' (_,y,_) = y

let thd (_,_,y) = y

let rec string_3_row_h clst acc =
  match clst with
  | [] -> acc
  | coords::t -> (*let coords = h.cell in*)
      let str = ("(")^(string_of_int (fst' coords))^", "^(string_of_int (snd' coords))^", "^(string_of_int (thd coords))^")"
      ^"   "^acc in
      string_3_row_h t str

let rec string_three_row clstlst acc =
  match clstlst with
  | [] -> acc
  | h::t -> let str = (string_3_row_h h "")^"\n"^acc in
    string_three_row t str

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "python"
  | Caml -> "caml"
  | None -> "none"

let rec iterate lst f =
  match lst with
  | [] -> ();
  | h::t -> (f h; iterate t f;)

let computer_move_st do_mode newSt =
  print_endline "Please wait while computer moves...";
  let playerr = string_of_player (State.curr_player newSt) in
  let comp_move =
    if game_level newSt = Easy then
      easy_ai_move newSt
    else if game_level newSt = Medium then
      medium_ai_move newSt
    else
      hard_ai_move newSt
  in
  let newSt' = do_mode comp_move newSt in
  let coords_move =
    begin
      match comp_move with
      | Place (a, b, c) -> (a, b, c)
      | _ -> failwith "Unimplemented"
    end
  in
  (if krazy_happ_st newSt' then
    ()
  else
    let x = fst (cell_coords_to_x_y coords_move) in
    let y = snd (cell_coords_to_x_y coords_move) in
    Graphics.synchronize();
    Graphics.remember_mode true;
    Gui.responsive_board playerr x y ;
    Gui.score (p1_score newSt') (p2_score newSt') ;
    print_board newSt';
    print_endline ("Score of player 1: "^(string_of_int (p1_score newSt'))^
                    "\n"^"Score of player 2: "^(string_of_int (p2_score newSt')))
  );
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
  | Quit -> raise Gui.Quit
  | Restart -> raise Gui.Restart
  | _ -> ended ()

(*[play st] is the helper function for play_game ()*)
let rec play single do_mode st=
  if game_ended st then
    let win_msg_and_stuff = get_result_message st in
    let win_msg = snd win_msg_and_stuff in
    print_endline win_msg;
    print_endline (fst win_msg_and_stuff);
    (Gui.winner_winner_chicken_dinner (fst win_msg_and_stuff));
    ended ()
  else
  (print_endline "Please enter command";
  let playerr = string_of_player (State.curr_player st ) in
  let p1_score0 = p1_score st in
  let p2_score0 = p2_score st in
  let hint_num = num_hints st in
  let try_num = num_tries st in
  let recent_wins = (most_recent_wins st ) in
  Gui.draw_act_two playerr p1_score0 p2_score0 hint_num try_num recent_wins;
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
  (* let st_modified = (  if playerr = "python" then (print_endline "python";print_int st.p1_num_tries;  {st with p1_num_tries = st.p1_num_tries - 1 })
                       else (print_endline "caml";print_int st.p2_num_tries;   {st with p2_num_tries = st.p2_num_tries - 1})) in *)
  if com = "try 1,1,1" then play single do_mode st else
    let command = parse com in
    Graphics.remember_mode true;
    Graphics.synchronize();
  let newSt = do_mode command st in
  match command with
  | Play str -> (print_endline "A game is currently is session. Please quit first.";
                 play single do_mode newSt)
  | Score ->
    (print_endline ("Score of player 1: "^(string_of_int (p1_score st))^"\n"^"Score of player 2: "^(string_of_int (p2_score st)));
  play single do_mode newSt)
  | Quit -> (raise Gui.Quit)
  | Restart -> (raise Gui.Restart)
  | Try (pl, x, y) -> (
      print_board newSt;
      print_endline "IN TRY";
      if newSt = st then (
        print_endline "POOP";
        let ex = snd test |> fst in
        let why = snd test |> snd in
        Gui.repeat_cell ex why;
        print_endline "Action impossible. Please try a different move.";
        if ((playerr = "python" && newSt.p1_num_tries = 0) ||(playerr = "caml" && newSt.p2_num_tries = 0)) then draw_image (Gui.get_img "imgs/tries_loss.jpg") 236 0;
        Graphics.remember_mode false;
        play single do_mode st;
      )
      else (
        let xx = snd test |> fst in
        let yy = snd test |> snd in
        Gui.cover_up ();
        print_int xx;
        print_int yy;
        let (clicked_accept, xa, ya) = Gui.try_responsive_board playerr xx yy (pl, x, y) in(* xx and yy are the locations to draw the image *)
        Gui.score (p1_score newSt) (p2_score newSt) ;
        Gui.num_try_hint (num_tries newSt) 836 587;
        Gui.num_try_hint (num_hints newSt) 171 593;
        if (clicked_accept) then (
          let place_st = do_mode (Place (pl, x, y)) newSt in
          if single then (
            let comp_st = computer_move_st do_mode place_st in play single do_mode comp_st
          )
          else (
            Graphics.remember_mode false;
            play single do_mode place_st
          )
        )
        else (
          let test = Gui.play_board "place" xa ya in
          let com = fst test in
          let command = parse com in
          let news = do_mode command newSt in
          if news = newSt then (
            let ex = snd test |> fst in
            let why = snd test |> snd in
            Gui.cover_up();
            Gui.repeat_cell ex why;
            print_endline "Action impossible. Please try a different move.";
            moveto (xx+15) (yy+4);
            Graphics.set_font "-*-fixed-medium-r-semicondensed--17-*-*-*-*-*-iso8859-1";
            Gui.cover_try playerr xx yy;
            Graphics.remember_mode false;
            play single do_mode news
          )
          else (
            let aa = snd test |> fst in
            let bb = snd test |> snd in
            Gui.responsive_board playerr aa bb;
            moveto (xx+15) (yy+4);
            Graphics.set_font "-*-fixed-medium-r-semicondensed--17-*-*-*-*-*-iso8859-1";
            Gui.cover_try playerr xx yy;
            Gui.cover_up();
            if single then
              (let comp_st = computer_move_st do_mode news in Graphics.remember_mode false;play single do_mode comp_st)
            else
              (Graphics.remember_mode false;play single do_mode news)
          )
        )
      )
    )
  | Place (pl, x, y) ->
    (if newSt = st then
       (let ex = snd test |> fst in
        let why = snd test |> snd in
        Gui.repeat_cell ex why;
        print_endline "Action impossible. Please try a different move.";
        Graphics.remember_mode false;
        play single do_mode newSt;)

     else
       (  (if not (krazy_happ_st newSt) then(
            print_board newSt;
            let x = snd test |> fst in
            let y = snd test |> snd in
            Gui.cover_up ();
            print_int x;
            print_int y;
            (*Graphics.synchronize(); Graphics.remember_mode true;*)
            Gui.responsive_board playerr x y ; (* x and y are the locations to draw the image *)
            Gui.score (p1_score newSt) (p2_score newSt) ;
            Gui.num_try_hint (num_tries newSt) 836 587;
            Gui.num_try_hint (num_hints newSt) 171 593;
            let recent_wins = (most_recent_wins newSt ) = [] in
            print_endline " ";print_endline "recent_wins is empty"; print_endline (string_of_bool recent_wins); print_endline " "
          )
          else
            ()
          );
          if single then
            (Graphics.remember_mode false;
              Gui.draw_wait_mgs();
              let comp_st = computer_move_st do_mode newSt in play single do_mode comp_st)
          else
            (Graphics.remember_mode false;play single do_mode newSt)
          ))
  | Hint ->

    let hint_move = player_hint newSt in
    Graphics.remember_mode false;
      let coord_move =
        begin
        match hint_move with
        | Place (a, b, c) -> (a, b, c)
        | _ -> failwith "Impossible"
        end
      in
      let x = fst (cell_coords_to_x_y coord_move) in
      let y = snd (cell_coords_to_x_y coord_move) in
      let newSt' = do_mode hint_move newSt in
      (if (not (krazy_happ_st newSt' )) then
         (
          Graphics.synchronize ();Graphics.remember_mode true;
          print_endline"did i call from here?";
        Gui.responsive_board playerr x y ; (* x and y are the locations to draw the image *)
        Gui.score (p1_score newSt) (p2_score newSt))
      else ());
      if single then
        (let comp_st = computer_move_st do_mode newSt' in Graphics.remember_mode false; play single do_mode comp_st)
      else
        (Graphics.synchronize(); Graphics.remember_mode false; play single do_mode newSt')
  | Look -> (print_board st; play single do_mode newSt)
  | CurrentPlayer ->
    (print_endline ("Current player: "^(string_of_player (curr_player st)));
     play single do_mode newSt)
  | Invalid -> (print_endline "Action impossible. Please try a different move.";
                play single do_mode st)
  )

let rec draw_all_moves cllst =
  match cllst with
  | [] -> ()
  | h::t -> let (x, y) = cell_coords_to_x_y (h.cell) in
      let plyr = string_of_player h.player in
      Gui.responsive_board plyr x y;
      draw_all_moves t

let do_kray_w_GUI (c:command) st =
  let st' = do_krazy c st in
  if krazy_happ_st st' then (
    (*redraw*)
    (if krazy_bomb_happ_st st' then (
      (*animation*)
      (* print_endline (string_three_row [List.map (fun a -> a.cell) (cells_occ st')] "");
      print_endline "++++++++++++++"; *)
    )
    else ());
      (*Act I*)
    clear_graph();
    draw_image (get_img "imgs/xxoo.jpg") 0 0;
    draw_image (get_img "imgs/TTTmain.jpg") 250 40;
    draw_image (get_img "imgs/hint.jpg") 800 555;
    draw_image (get_img "imgs/try.jpg") 134 555;
    draw_image(get_img "imgs/quit.jpg") 850 313;
    draw_image(get_img "imgs/restart.jpg") 50 313;

    (*Intermission*)
    cells_occ st' |> draw_all_moves;

    (*Act 2*)
    let playerr = curr_player st'|> string_of_player in
    let p1_sc = p1_score st' in
    let p2_sc = p2_score st' in
    let hint_num = num_hints st' in
    let num_trys = num_tries st' in
    let recent_wins_lst = most_recent_wins st' in
    draw_act_two playerr p1_sc p2_sc hint_num num_trys recent_wins_lst
  )
  else (
    ()
  );
  st'

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
        play (game_num_plyrs init_st <> Multi)
            (if game_mode init_st = Krazy then do_kray_w_GUI else do') init_st
      ) with
    | Gui.Quit -> print_endline "Bye!"; exit 0;
    | Gui.Restart -> (print_endline "You have chosen to restart this game";
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
