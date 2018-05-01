open Graphics
open State
(* open Camlimages *)


let init_welcome f =
  print_int([|[|black|];[|black|]|] |> Array.length);
  (*  let img = [|[|black|];[|black|]|]|> make_image in*)
  (* let img = "imgs/wilkommen.png" in
  let i =  Png.load img [] |> Graphic_image.array_of_image |> make_image in
  Graphics.draw_image  i 1000 750;
  moveto 0 0 *)
