open Graphics
open State
open Camlimages
open Images
open Jpeg
open Command
open Types


(* make the black colored backround color *)
let bblack = rgb 32 32 32

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

  let event_lst = [Graphics.Button_down] in
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
  (draw_image (get_img "imgs/coverup.jpg")236 3;)

let play_board () =
  let event_lst = [Graphics.Button_up] in
  let mouse_status = wait_next_event event_lst  in
  let x = mouse_status.mouse_x in
  let y = mouse_status.mouse_y in
  print_string "x is: "; print_int x; print_endline " ";
  print_string "y is: "; print_int y; print_endline " ";
  print_endline " ";

  if ((x >= 331 && x <=426) && (y >= 651 && y <= 685 )) then ("place 0,0,0", (360, 666))
  else if ((x >= 438 && x <= 575) && (y >= 653 && y <= 685 )) then ("place 0,0,1", (475, 666))
  else if ((x >= 580 && x <= 696) && (y >= 651 && y <= 684)) then ("place 0,0,2", (605, 666))
  else if ((x >= 296 && x <= 414) && (y >= 596 && y <= 645)) then ("place 0,1,0", (340, 610))
  else if ((x >= 422 && x <= 574) && (y >= 597 && y <= 644)) then ("place 0,1,1", (475, 615))
  else if ((x >= 585 && x <= 697) && (y >= 597 && y <= 645)) then ("place 0,1,2", (625, 610))
  else if ((x >= 251 && x <= 401) && (y >= 541 && y <= 590)) then ("place 0,2,0", (320, 550))
  else if ((x >= 418 && x <= 576) && (y >= 540 && y <= 590)) then ("place 0,2,1", (475, 555))
  else if ((x >= 583 && x <= 760) && (y >= 540 && y <= 590)) then ("place 0,2,2", (645, 555))

  else if ((x >= 319 && x <= 428) && (y >= 480 && y <= 520)) then ("place 1,0,0", (350, 492))
  else if ((x >= 439 && x <= 575) && (y >= 496 && y <= 524)) then ("place 1,0,1", (475, 492))
  else if ((x >= 577 && x <= 670) && (y >= 493 && y <= 528)) then ("place 1,0,2", (605, 492))
  else if ((x >= 300 && x <= 414) && (y >= 437 && y <= 489)) then ("place 1,1,0", (340, 440))
  else if ((x >= 433 && x <= 575) && (y >= 440 && y <= 490)) then ("place 1,1,1", (0,0))
  else if ((x >= 583 && x <= 726) && (y >= 439 && y <= 489)) then ("place 1,1,2", (625, 440))
  else if ((x >= 251 && x <= 401) && (y >= 384 && y <= 434)) then ("place 1,2,0", (320, 385))
  else if ((x >= 418 && x <= 579) && (y >= 383 && y <= 432)) then ("place 1,2,1", (475, 390))
  else if ((x >= 583 && x <= 763) && (y >= 381 && y <= 432)) then ("place 1,2,2", (645, 385))

  else if ((x >= 316 && x <= 427) && (y >= 324 && y <= 361)) then ("place 2,0,0", (360, 330))
  else if ((x >= 440 && x <= 575) && (y >= 324 && y <= 360)) then ("place 2,0,1", (475, 330))
  else if ((x >= 580 && x <= 694) && (y >= 323 && y <= 360)) then ("place 2,0,2", (605, 330))
  else if ((x >= 284 && x <= 414) && (y >= 269 && y <= 322)) then ("place 2,1,0", (340, 280))
  else if ((x >= 430 && x <= 576) && (y >= 270 && y <= 318)) then ("place 2,1,1", (475, 275))
  else if ((x >= 582 && x <= 730) && (y >= 270 && y <= 318)) then ("place 2,1,2", (625, 280))
  else if ((x >= 252 && x <= 404) && (y >= 216 && y <= 265)) then ("place 2,2,0", (320, 225))
  else if ((x >= 420 && x <= 580) && (y >= 215 && y <= 265)) then ("place 2,2,1", (475, 225))
  else if ((x >= 585 && x <= 763) && (y >= 216 && y <= 266)) then ("place 2,2,2", (645, 225))

  else ("place 1,1,1", (1,1))

let repeat_cell x y =
  if x = 0 && y = 0 then (draw_image (get_img "imgs/no_x.jpg") 236 3;)
  else if x = 1 && y = 1 then (draw_image (get_img "imgs/stay.jpg") 236 3;)
  else (draw_image (get_img "imgs/msg2.jpg") 236 3;)


let responsive_board str x y =
  print_string "ex is: "; print_int x;
  print_endline " ";
  print_string "why is: "; print_int y;
  let file_name = "imgs/" ^ str ^ ".jpg" in
  (draw_image (get_img file_name ) x y;)

let highlight_curr_player str =
  if str = "python" then (rect_drawn_bblack 596 150 64 60; rect_drawn_cyan 334 145 62 65;)
  else (rect_drawn_bblack 334 145 62 65; rect_drawn_cyan 596 150 64 60;)

let winner_winner_chicken_dinner str =
  print_endline "in winner winner chicken dinner";
  draw_image (get_img "imgs/win.jpg") 200 200;
  if str = "win" then ((draw_image (get_img "imgs/win.jpg") 200 200);)
  else if str = "draw" then ((draw_image (get_img "imgs/draw_img.jpg") 200 200);)
  else if str = "lost" then ((draw_image (get_img "imgs/loss_img.jpg") 200 200);)
  else set_color magenta; draw_string "bruhhhh";
