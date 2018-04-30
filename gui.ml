open Graphics
open State

let init_welcome f =
  draw_image (get_img "imgs/wilkommen.png") 0 0;
  moveto 0 0
