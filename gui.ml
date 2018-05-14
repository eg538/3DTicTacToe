open Graphics
open State
open Camlimages
open Images
open Jpeg
open Command
open Types
open Grid_3d


(* make the black colored backround color *)
let bblack = rgb 32 32 32

(* make the color for score text *)
let score_color = rgb 0 255 205

(* make color for rectangles *)
let gray = rgb 151 151 151

(* make color for try*)
let annoying_green = rgb 0 255 0

(* make color for top plane *)
let top_plep = rgb 23 45 183

(* make color for middle plane *)
let mid_plep = rgb 71 0 140

(* make color for bottom plane*)
let bot_plep = rgb 139 37 61

(* [round (x, y) transforms the floating point values of (x, y) into ints. ]*)
let round (x, y) = int_of_float x, int_of_float y

(* [round_list lst transforms the floating point values of lst into ints. ]*)
let rec round_list = function
  | [] -> []
  | h :: t -> (h |> fst |> int_of_float, h |> snd |> int_of_float) :: round_list t

(* [array_of_image img] transforms a given image to a color color array. *)
let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
    let w = bitmap.Index8.width
    and h = bitmap.Index8.height
    and colormap = bitmap.Index8.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> Graphics.rgb r g b) colormap in
    if bitmap.Index8.transparent <> -1 then
      cmap.(bitmap.Index8.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))
  | Index16 bitmap ->
    let w = bitmap.Index16.width
    and h = bitmap.Index16.height
    and colormap = bitmap.Index16.colormap.map in
    let cmap = Array.map (fun {r = r; g = g; b = b} -> rgb r g b) colormap in
    if bitmap.Index16.transparent <> -1 then
      cmap.(bitmap.Index16.transparent) <- transp;
    Array.init h (fun i ->
        Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
    let w = bitmap.Rgb24.width
    and h = bitmap.Rgb24.height in
    Array.init h (fun i ->
        Array.init w (fun j ->
            let {r = r; g = g; b = b} = Rgb24.unsafe_get bitmap j i in
            rgb r g b))
  | Rgba32 _ -> failwith "RGBA not supported"
  | Cmyk32 _ -> failwith "CMYK not supported"

(* [get_img img] returns an image according to input file name. *)
let get_img img =
  Jpeg.load img [] |> array_of_image |> make_image

let rect_drawn x y width height =
  (set_color magenta;
    set_line_width 9;
   draw_rect x y width height;)

let rect_drawn_white x y width height =
  (set_color white;
   set_line_width 9;
   draw_rect x y width height;)

let rect_drawn_cyan x y width height =
  (set_color cyan;
   set_line_width 9;
   draw_rect x y width height;)

let rect_drawn_bblack x y width height =
  (set_color bblack;
  set_line_width 9;
   draw_rect x y width height;)

let rect_drawn_gray x y width height =
  (set_color gray;
   set_line_width 2;
  draw_rect x y width height)



type choices = {mode : string; level : string; num_p: string; }

let get_coordinates_choices c =
  if c = "easy" then rect_drawn_white 310 260 108 44
  else if c = "medium" then rect_drawn_white 450 260 119 43
  else if c = "hard" then rect_drawn_white 600 260 81 43
  else if c = "normal" then rect_drawn_white 380 200 109 43
  else if c = "krazy" then rect_drawn_white 520 200 93 44
  else if c = "single" then rect_drawn_white 395 130 93 44
  else if c = "multi" then rect_drawn_white 520 130 82 44
  else ()

let rec get_choices ch =
let event_lst = [Graphics.Button_up] in
let mouse_status = wait_next_event event_lst  in
let x = mouse_status.mouse_x in
let y = mouse_status.mouse_y in

if (x >= 310 && x <= (310 + 108)) && (y >= 260 && y <= (260 + 44))
then (get_coordinates_choices ch.level;(rect_drawn 310 260 108 44); get_choices {ch with level = "easy"} )
else if (x >= 450 && x <= (450 + 119)) && (y >= 260 && y <= (260 + 43))
then (get_coordinates_choices ch.level;(rect_drawn 450 260 119 43); get_choices {ch with level ="medium"})
else if (x >= 600 && x <= (600 + 81)) && (y >= 260 && y <= (260 + 43))
then (get_coordinates_choices ch.level;(rect_drawn 600 260 81 43); get_choices {ch with level ="hard"})
else if (x >= 380 && x <= (380 + 109)) && (y >= 200 && y <= (200 + 43))
then (get_coordinates_choices ch.mode;(rect_drawn 380 200 109 43); get_choices {ch with mode ="normal"})
else if (x >= 520 && x <= (520 + 93)) && (y >= 200 && y <= (200 + 44))
then (get_coordinates_choices ch.mode;(rect_drawn 520 200 93 44); get_choices {ch with mode ="krazy"})
else if (x >= 395 && x <= (395 + 93)) && (y >= 130 && y <= (130 + 44))
then (get_coordinates_choices ch.num_p;(rect_drawn 395 130 93 44);get_choices{ch with num_p = "single"})
else if (x >= 520 && x <= (520 + 82)) && (y >= 130 && y <= (130 + 44))
then (get_coordinates_choices ch.num_p;(rect_drawn 520 130 82 44); get_choices{ch with num_p ="multi"})
else if (not(x >= 380 && x <=(242+380)) && (y>=35 && y<=(35+69)))
then get_choices ch
else ch


let rec start_game ch =

  let event_lst = [Graphics.Button_up] in
  let mouse_status = wait_next_event event_lst  in
  let x = mouse_status.mouse_x in
  let y = mouse_status.mouse_y in
  if(x >= 380 && x <=(242+380)) && (y>=35 && y<=(35+69)) then(
    let play_str = "play " ^ ch.num_p ^ " " ^ "python " ^ ch.level ^ " " ^ ch.mode in
    rect_drawn x y 90 90; clear_graph(); draw_image (get_img "imgs/xxoo.jpg") 0 0;draw_image (get_img "imgs/TTTmain.jpg") 250 40;
    draw_image(get_img "imgs/hint.jpg") 800 555; draw_image(get_img "imgs/try.jpg") 134 555;
    play_str) else (
    let event_lst = [Graphics.Button_up] in
    let mouse_status = wait_next_event event_lst  in
    let x = mouse_status.mouse_x in
    let y = mouse_status.mouse_y in
    let choice = get_choices ch in
    if(x >= 380 && x <=(242+380)) && (y>=35 && y<=(35+69)) then(
      let play_str = "play " ^ choice.num_p ^ " " ^ "python " ^ choice.level ^ " " ^ choice.mode in
      rect_drawn x y 90 90; clear_graph(); draw_image (get_img "imgs/xxoo.jpg") 0 0;draw_image (get_img "imgs/TTTmain.jpg") 250 40;
      draw_image(get_img "imgs/hint.jpg") 800 555; draw_image(get_img "imgs/try.jpg") 134 555;
      play_str)
    else (start_game choice))


let init_welcome () =
  draw_image (get_img "imgs/background_crop.jpeg") 0 0;

  draw_image (get_img "imgs/wilkommen-logo.jpg") 270 330;

  draw_image (get_img "imgs/easy.jpg") 310 260;
  rect_drawn 310 260 108 44;

  draw_image (get_img "imgs/medium.jpg") 450 260;
  rect_drawn_white 450 260 119 43;

  draw_image (get_img "imgs/hard.jpg") 600 260;
  rect_drawn_white 600 260 81 43;

  draw_image (get_img "imgs/g.jpg") 380 200;
  rect_drawn 380 200 109 43;

  draw_image (get_img "imgs/krazy.jpg") 520 200;
  rect_drawn_white 520 200 93 44;

  draw_image (get_img "imgs/single.jpg") 395 130;
  rect_drawn_white 395 130 93 44;

  draw_image (get_img "imgs/multi.jpg") 520 130;
  rect_drawn 520 130 82 44;

  draw_image (get_img "imgs/group.jpg") 380 35;
  let ch = {mode = "normal"; level = "easy"; num_p= "multi"; } in
  start_game ch

let cover_up () =
  draw_image (get_img "imgs/coverup.jpg") 0 11;
  draw_image (get_img "imgs/coverup.jpg") 0 3;
  draw_image (get_img "imgs/coverup.jpg") 500 11;
  (draw_image (get_img "imgs/coverup.jpg")236 3;)


let score p1 p2 =
  draw_image (get_img "imgs/eraser.jpg") 330 42;
  draw_image (get_img "imgs/eraser.jpg") 590 42;
  print_endline "in score";
  let juan = string_of_int p1 in
  moveto 352 72;
  set_color white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--55-*-*-*-*-*-iso8859-1";
  draw_string juan;
  let tiu = string_of_int p2 in
  moveto 617 72;
  (draw_string tiu;)


let num_try_hint num x y =
  let str = string_of_int num in
  draw_image (get_img "imgs/eraser.jpg") (x-24) (y-52);
  moveto x y;
  set_color white;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--35-*-*-*-*-*-iso8859-1";
  (draw_string str;)

  (* let file_name = str ^ "_try.jpg" in
  (draw_image (get_img ("imgs/"^file_name)) why ex;) *)

  let fst' (y,_,_) = y

  let snd' (_,y,_) = y

  let thd (_,_,y) = y

let which_command () =
    let event_lst = [Graphics.Button_up] in
    let mouse_status = wait_next_event event_lst  in
    let x = mouse_status.mouse_x in
    let y = mouse_status.mouse_y in
    print_string "x is: "; print_int x; print_endline " ";
    print_string "y is: "; print_int y; print_endline " ";
    print_endline " ";

    if (x >= 149 && x <= (149 + 69)) && (y >= 627 && y<= (627 + 44)) then
      ( let ev = [Graphics.Button_up] in
        let ms = wait_next_event ev in
        let xx =  ms.mouse_x in
        let yy = ms.mouse_y in
        ("try" , xx, yy))
    else ("place" , x, y)


let play_board command x y =
  (* let event_lst = [Graphics.Button_up] in
  let mouse_status = wait_next_event event_lst  in
  let x = mouse_status.mouse_x in
  let y = mouse_status.mouse_y in
  print_string "x is: "; print_int x; print_endline " ";
  print_string "y is: "; print_int y; print_endline " ";
  print_endline " "; *)

  (* let command = fst' input in
  let x = snd' input in
  let y = thd input in  *)

  if ((x >= 331 && x <=426) && (y >= 651 && y <= 685 )) then ((command^" 0,0,0"), (360, 666))
  else if ((x >= 438 && x <= 575) && (y >= 653 && y <= 685 )) then ((command^" 0,0,1"), (475, 666))
  else if ((x >= 580 && x <= 696) && (y >= 651 && y <= 684)) then ((command^" 0,0,2"), (605, 666))
  else if ((x >= 296 && x <= 414) && (y >= 596 && y <= 645)) then ((command^" 0,1,0"), (340, 610))
  else if ((x >= 422 && x <= 574) && (y >= 597 && y <= 644)) then ((command^" 0,1,1"), (475, 615))
  else if ((x >= 585 && x <= 697) && (y >= 597 && y <= 645)) then ((command^" 0,1,2"), (625, 610))
  else if ((x >= 251 && x <= 401) && (y >= 541 && y <= 590)) then ((command^" 0,2,0"), (320, 550))
  else if ((x >= 418 && x <= 576) && (y >= 540 && y <= 590)) then ((command^" 0,2,1"), (475, 555))
  else if ((x >= 583 && x <= 760) && (y >= 540 && y <= 590)) then ((command^" 0,2,2"), (645, 555))

  else if ((x >= 319 && x <= 428) && (y >= 480 && y <= 520)) then ((command^" 1,0,0"), (350, 492))
  else if ((x >= 439 && x <= 575) && (y >= 496 && y <= 524)) then ((command^" 1,0,1"), (475, 492))
  else if ((x >= 577 && x <= 670) && (y >= 493 && y <= 528)) then ((command^" 1,0,2"), (605, 492))
  else if ((x >= 300 && x <= 414) && (y >= 437 && y <= 489)) then ((command^" 1,1,0"), (340, 440))
  else if ((x >= 433 && x <= 575) && (y >= 440 && y <= 490)) then ((command^" 1,1,1"), (0,0))
  else if ((x >= 583 && x <= 726) && (y >= 439 && y <= 489)) then ((command^" 1,1,2"), (625, 440))
  else if ((x >= 251 && x <= 401) && (y >= 384 && y <= 434)) then ((command^" 1,2,0"), (320, 385))
  else if ((x >= 418 && x <= 579) && (y >= 383 && y <= 432)) then ((command^" 1,2,1"), (475, 390))
  else if ((x >= 583 && x <= 763) && (y >= 381 && y <= 432)) then ((command^" 1,2,2"), (645, 385))

  else if ((x >= 316 && x <= 427) && (y >= 324 && y <= 361)) then ((command^" 2,0,0"), (360, 330))
  else if ((x >= 440 && x <= 575) && (y >= 324 && y <= 360)) then ((command^" 2,0,1"), (475, 330))
  else if ((x >= 580 && x <= 694) && (y >= 323 && y <= 360)) then ((command^" 2,0,2"), (605, 330))
  else if ((x >= 284 && x <= 414) && (y >= 269 && y <= 322)) then ((command^" 2,1,0"), (340, 280))
  else if ((x >= 430 && x <= 576) && (y >= 270 && y <= 318)) then ((command^" 2,1,1"), (475, 280))
  else if ((x >= 582 && x <= 730) && (y >= 270 && y <= 318)) then ((command^" 2,1,2"), (625, 280))
  else if ((x >= 252 && x <= 404) && (y >= 216 && y <= 265)) then ((command^" 2,2,0"), (320, 225))
  else if ((x >= 420 && x <= 580) && (y >= 215 && y <= 265)) then ((command^" 2,2,1"), (475, 220))
  else if ((x >= 585 && x <= 763) && (y >= 216 && y <= 266)) then ((command^" 2,2,2"), (645, 225))

  else (command^" 1,1,1", (1,1))

let repeat_cell x y =
  if x = 0 && y = 0 then (draw_image (get_img "imgs/no_x.jpg") 236 0;)
  else if x = 1 && y = 1 then (draw_image (get_img "imgs/stay.jpg") 236 0;)
  else (sound 440 1000; draw_image (get_img "imgs/msg2.jpg") 236 0;)

let responsive_board str x y =
  (let file_name = "imgs/" ^ str ^ ".jpg" in
   print_endline file_name;
   draw_image (get_img file_name ) x y;)

let choose_letter str =
  if str = "python" then (draw_string "P";)
  else (draw_string "C";)

let cover_try str ex why =
  if (why >= 500 && why <= 666) then
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

let try_responsive_board str x y (pl, ex, why)=
    moveto (x+15) (y+4);
    set_color annoying_green;
    Graphics.set_font "-*-fixed-medium-r-semicondensed--17-*-*-*-*-*-iso8859-1";
     choose_letter str;
    draw_image (get_img "imgs/accept.jpg") 65 22;
    let event_lst = [Graphics.Button_up] in
     let mouse_status = wait_next_event event_lst  in
     let xa = mouse_status.mouse_x in
     let ya = mouse_status.mouse_y in
     let (sy, (a, b)) = play_board "try" xa ya in
     let c1 = String.index sy ',' in
    let p = int_of_char (String.get sy (c1 - 1)) - 48 in
    print_endline(string_of_bool (p = pl));
    let xx = int_of_char (String.get sy (c1 + 1)) - 48 in
    print_endline(string_of_bool (ex = xx));
    let yy = int_of_char (String.get sy (c1 + 3)) - 48 in
    print_endline(string_of_bool (why = yy));
    if ((p = pl && ex = xx && why = yy)||((xa >= 66 && xa <= 198)&& (ya >= 22 && ya <= 78) )) then
      ( if (p = pl && ex = xx && why = yy) then (responsive_board str a b; cover_up(); (true, a, b);)else
          (responsive_board str a b; cover_up();(true, a, b);)) else ((false, xa, ya);)

(*let choose_letter str =
  if str = "python" then (draw_string "P";)
  else (draw_string "C";)

let cover_try str ex why =
  if (why >= 500 && why <= 666) then
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


*)

let highlight_curr_player str =
  if str = "python" then (rect_drawn_bblack 596 150 64 60; rect_drawn_cyan 334 145 62 65;)
  else (rect_drawn_bblack 334 145 62 65; rect_drawn_cyan 596 150 64 60;)

let winner_winner_chicken_appetizer str =
  draw_image (get_img str) 200 200;
  draw_image (get_img "imgs/quit.jpg") 225 425;
  (draw_image (get_img "imgs/restart.jpg") 225 325;)


let winner_winner_chicken_dinner str =
  if str = "win" then (winner_winner_chicken_appetizer "imgs/win.jpg";)
  else if str = "lost" then (winner_winner_chicken_appetizer "imgs/loss_img.jpg";)
  else if str = "caml" then (winner_winner_chicken_appetizer "imgs/caml_wins.jpg";)
  else if str = "python" then (winner_winner_chicken_appetizer "imgs/python_wins.jpg";)
  else (winner_winner_chicken_appetizer "imgs/draw_img.jpg";)

let mark_three x y =
  set_color annoying_green;
  Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  (draw_string "X";)

let cell_coords_to_x_y (pl, x, y)=
  match (pl,x,y) with
  | (0,0,0) -> (360, 666)
  | (0,0,1) -> (475, 666)
  | (0,0,2) -> (605, 666)
  | (0,1,0) -> (340, 610)
  | (0,1,1) -> (475, 615)
  | (0,1,2) -> (625, 610)
  | (0,2,0) -> (320, 550)
  | (0,2,1) -> (475, 555)
  | (0,2,2) -> (645, 555)
  | (1,0,0) -> (350, 492)
  | (1,0,1) -> (475, 492)
  | (1,0,2) -> (605, 492)
  | (1,1,0) -> (340, 440)
  | (1,1,1) -> (0,0)
  | (1,1,2) -> (625, 440)
  | (1,2,0) -> (320, 385)
  | (1,2,1) -> (475, 390)
  | (1,2,2) -> (645, 385)
  | (2,0,0) -> (360, 330)
  | (2,0,1) -> (475, 330)
  | (2,0,2) -> (605, 330)
  | (2,1,0) -> (340, 280)
  | (2,1,1) -> (475, 275)
  | (2,1,2) -> (625, 280)
  | (2,2,0) -> (320, 225)
  | (2,2,1) -> (475, 225)
  | (2,2,2) -> (645, 225)
  | _ -> failwith "Nop"

(* let draw_three_row_helper_helper lst x y =
  match lst with
  | [] ->
  | (pl,x,y)::t -> *)

(* let draw_three_row_helper hd_lst tl_lst =
  match hd_lst with
  | [] -> line_drawn
  | lst -> draw_three_row_helper_helper (List.sort Pervasives.compare lst ) (); *)

let draw_three_row recent_wins =
  print_endline " "; print_endline "in Gui.draw_three_row"; print_endline " ";
  match recent_wins with
  | [] -> print_endline "impossible. shouldn't be given an empty list";
  | h::t -> print_endline "yay";
