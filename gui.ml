open Graphics
open State

let init_welcome f =
  print_int([|[|black|];[|black|]|] |> Array.length);
  let img = [|[|black|];[|black|]|]|> make_image in
  draw_image  img 1000 750;
  moveto 0 0
