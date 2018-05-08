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

 let rect_drawn x y width height =
  (set_color magenta;
    set_line_width 9;
    draw_rect x y width height;)

(* let react x_min y_min width height =

    let event_lst = [Graphics.Button_up] in
    let mouse_status = wait_next_event event_lst  in
    let x = mouse_status.mouse_x in
    let y = mouse_status.mouse_y in
    if (x >= x_min && x <= (x_min + width)) && (y >= y_min && y <= (y_min + height)) then (rect_drawn x_min y_min width height;)  *)


let rec reaction =
  let event_lst = [Graphics.Button_up] in
  let mouse_status = wait_next_event event_lst  in
  let x = mouse_status.mouse_x in
  let y = mouse_status.mouse_y in

    (* easy button *)
    if (x >= 310 && x <= (310 + 260)) && (y >= 107 && y <= (107 + 44)) then (rect_drawn 310 107 260 44; reaction;)

    (* medium button *)
    else if (x >= 450 && x <= (450 + 119)) && (y >= 260 && y <= (260 + 43)) then (rect_drawn 450 260 119 43; reaction;)

    (*hard button pressed *)
    else if (x >= 600 && x <= (600 + 81)) && (y >= 260 && y <= (260 + 43)) then (rect_drawn 600 260 81 43; reaction;)

    (* normal button pressed *)
    else if (x >= 280 && x <= (280 + 109)) && (y >= 200 && y <= (200 + 43)) then (rect_drawn 380 200 109 43; reaction;)

    (* krazy button pressed *)
    else if (x >= 520 && x <= (520 + 93)) && (y >= 200 && y <= (200 + 44)) then (rect_drawn 520 200 93 44; reaction;)

    (* single button pressed *)
    else if (x >= 395 && x <= (395 + 93)) && (y >= 130 && y <= (130 + 44)) then (rect_drawn 395 130 93 44; reaction;)

    (* multi button pressed *)
    else if (x >= 520 && x <= (520 + 82)) && (y >= 130 && y <= (130 + 44)) then (rect_drawn 520 130 82 44; reaction;)

    (* start was hit*)
    else if (x >= 380 && x <=(242+380)) && (y>=35 && y<=(35+69)) then
      (clear_graph(); draw_image (get_img "imgs/xxoo.jpg") 0 0;draw_image (get_img "imgs/TTTmain.jpg") 250 40;
       draw_image(get_img "imgs/hint.jpg") 800 555; draw_image(get_img "imgs/try.jpg") 134 555;)

    (* nothing was hit *)
    else reaction;

let init_welcome f =
  draw_image (get_img "imgs/background_crop.jpeg") 0 0;

  draw_image (get_img "imgs/wilkommen-logo.jpg") 270 330;

  draw_image (get_img "imgs/easy.jpg") 310 260;

  draw_image (get_img "imgs/medium.jpg") 450 260;

  draw_image (get_img "imgs/hard.jpg") 600 260;

  draw_image (get_img "imgs/g.jpg") 380 200;

  draw_image (get_img "imgs/krazy.jpg") 520 200;

  draw_image (get_img "imgs/single.jpg") 395 130;

  draw_image (get_img "imgs/multi.jpg") 520 130;

  draw_image (get_img "imgs/group.jpg") 380 35;










  (* a;lsdkf
  (* easy button pressed *)
  react 310 260 107 44;

  (* medium button pressed *)
  react 450 260 119 43;

  (*hard button pressed *)
  react 600 260 81 43;

  (* normal button pressed *)
  react 380 200 109 43;

  (* krazy button pressed *)
  react 520 200 93 44;

  (* single button pressed *)
  react 395 130 93 44;

  (* multi button pressed *)
  react 520 130 82 44;

;lsdkf *)
