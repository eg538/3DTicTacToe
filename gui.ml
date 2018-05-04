open Graphics
open State
open Camlimages
open Images
open Jpeg


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

(*let init_welcome f =
  print_int([|[|black|];[|black|]|] |> Array.length);
  (*  let img = [|[|black|];[|black|]|]|> make_image in*)
  let img = "imgs/wilkommen.png" in
  let i =  Png.load img [] |> Graphic_image.array_of_image |> make_image in
  Graphics.draw_image  i 1000 750;
  moveto 0 0*)

let init_welcome f =
  draw_image (get_img "imgs/background.jpg") 0 0;
  draw_image (get_img "imgs/resized_wilkommen_main.jpg") 200 100;
  (* moveto 60 260; *)
  (*draw_image(get_img "imgs/Rectangle.jpg") 90 100;*)
  (* let pressed = button_down () in *)
  (* let pos = mouse_pos () in
  let x = fst pos in
  let y = snd pos in
     if pressed then *)

  draw_image (get_img "imgs/start.jpg") 420 20;
  let x_wilkommen = 200 in
  let y_wilkommen = 100 in

  let easy_x_pos = 548 in
  let easy_y_pos = 383 in
  let easy_width = 45 in
  let easy_height = 24 in
  let easy_x_min = x_wilkommen + easy_x_pos in
  let easy_x_max = easy_x_min + easy_width in
  let easy_y_min = y_wilkommen + easy_y_pos in
  let easy_y_max = easy_y_min + easy_height in

  let event_lst = [Graphics.Button_up] in
  let mouse_status = wait_next_event event_lst  in
  let x = mouse_status.mouse_x in
  let y = mouse_status.mouse_y in
  if (x >= 420 && x <= 618) && (y >= 20 && y <= 89) then 
  (clear_graph(); draw_image (get_img "imgs/xxoo.jpg") 0 0;draw_image (get_img "imgs/TTTmain.jpg") 250 40; 
    draw_image(get_img "imgs/hint.jpg") 800 555; draw_image(get_img "imgs/try.jpg") 134 555;)
       else draw_rect 250 150 90 90;
