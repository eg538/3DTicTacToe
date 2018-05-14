let choose_letter str =
  if str = "python" then (draw_string "P";)
else (draw_string "C";)

let cover_try str ex why =
  if (why >= 555 && why <= 666) then
    (moveto (ex+15) (why+4);
      set_color top_plep;
     choose_letter str
    )
  else if (why >= 385 && why <= 492) then
    (moveto (ex+15) (why+4);
      set_color mid_plep;
     choose_letter str
    )
  else if (why >= 225 && why <= 330) then
    (moveto (ex+15) (why+4);
      set_color bot_plep;
     choose_letter str
    )
  else ()

let change_curr_player st=
  if st.current_player = Python then {st with current_player = Caml}
  else {st with current_player = Python}

let tried str ex why st =
  let (sy, (a,b)) = play_board "try" ex why in
  let c1 = String.index sy ',' in
  let pl = int_of_char (String.get sy (c1 - 1)) - 48 in
  let xx = int_of_char (String.get sy (c1 + 1)) - 48 in
  let yy = int_of_char (String.get sy (c1 + 3)) - 48 in
  print_endline"what?";
  print_int pl; print_int xx;print_int yy;
  print_endline"the hell?";
  let check = Grid_3d.is_taken (pl, xx, yy) st.tttBoard in
  print_endline (string_of_bool check);
  if (check) then
    (
      if(a <= 1 && b <= 1) then
        (repeat_cell a b; change_curr_player st)
      else
      (moveto (ex+15) (why+4);
      set_color annoying_green;
      Graphics.set_font "-*-fixed-medium-r-semicondensed--17-*-*-*-*-*-iso8859-1";
      choose_letter str;
       draw_image (get_img "imgs/accept.jpg") 65 22;
      let event_lst = [Graphics.Button_up] in
      let mouse_status = wait_next_event event_lst  in
      let x = mouse_status.mouse_x in
      let y = mouse_status.mouse_y in
      if ((x >= 66 && x <= 198)&& (y >= 22 && y <= 78)) then
        ( print_endline "is it here?";
          let (s, (a,b)) = play_board "place" ex why in
          cover_try str ex why;
          responsive_board str a b;
          cover_up();
          (*let c1 = String.index s ',' in
            let pl = String.get s (c1 - 1) in
            let xx = String.get s (c1 + 1) in
            let yy = String.get s (c1 + 3) in
            State.do' (Place(pl, xx, yy)) st
            play_move st (pl, xx, yy) |> switch_players |> check_game_end*)
          let comm = Command.parse s in
          State.do' comm (st)
        )
      else
        ( cover_try str ex why;
          cover_up();
          let (s, (a,b)) = play_board "place" x y in
          responsive_board str a b;
          let comm = Command.parse s in
          let new_st = State.do' comm (st) in
          new_st
        )
      ))else   ( draw_image(get_img "imgs/msg2.jpg") 236 0; st)
