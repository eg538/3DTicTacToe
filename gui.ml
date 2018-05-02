open Graphics
open State
(* open Camlimages *)


let init_welcome f =
  print_int([|[|black|];[|black|]|] |> Array.length);
  (*  let img = [|[|black|];[|black|]|]|> make_image in*)
  (* let img = "imgs/wilkommen.png" in
  let i =  Png.load img [] |> Graphic_image.array_of_image |> make_image in
     Graphics.draw_image  i 1000 750; *)
  open_graph " 1000x750";
  moveto 130 620;
  set_font "-*-fixed-medium-r-semicondensed--75-*-*-*-*-*-iso8859-1";
  draw_string "3D";
  let x = current_x () in
  let y = current_y () in
  draw_rect (x+30) (y-400) 10 400;
