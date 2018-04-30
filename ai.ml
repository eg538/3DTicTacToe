open State
open Grid_3d
open Parse_init
(* type valid_node = {board: cell list -> cell list -> cell list; mutable move: int*int*int} *)

(*type 'a stream = Cons of 'a * (unit -> 'a stream)*)

(* type nodeStream = Cons of int * (unit -> nodeStream) *)
(* type node = {available: cell list; taken: cell list; hVal: int}
type gsTree = Leaf of int * int * int | Node of node * (gsTree list) *)
type gsTree = Leaf of int*int*int | Node of board * (gsTree list)

let switch_plyr p = match p with
  | Python -> Caml
  | Caml -> Python

let rec placement_helper f b remaining_cells plyr d =
  match remaining_cells with
  | [] -> []
  | h::t -> let cpy = copy b in
    place (cell_coords h) cpy plyr;
    (f cpy plyr d)::(placement_helper f b t plyr d)

let rec gt_gen_help b plyr d =
  print_string (asciiBoard b);
  print_endline "*****************";
  let remaining_cells = cells_left b in
  let occupied_cells = cells_occupied b in
  let nd = {available = remaining_cells; taken = occupied_cells; hVal = 0} in
  if List.length remaining_cells > 0 && d <> 0 then
    (* Node (nd, placement_helper gt_gen_help b remaining_cells (switch_plyr plyr) (d - 1)) *)
    Node (b, placement_helper gt_gen_help b remaining_cells (switch_plyr plyr) (d - 1))
  else
    (* Node (nd, [Leaf (0, 0, 0)]) *)
    Node (b, [Leaf (0, 0, 0)])

let game_tree_generate st d = gt_gen_help (board st) (curr_player st) d

let rec print_head t =
  match t with
  | Leaf (_, _, _) -> print_endline ""
  | Node (b, children) -> print_string (asciiBoard b)

let rec print_children t =
  match t with
  | Leaf (_, _, _) -> print_endline ""
  | Node (b, c) -> List.iter (fun a -> print_head a) c

(*[available_cells st] is the list of cells that are vacant in the 3D game board
 * that st contains*)
(*let available_cells st =
  let positions = [(0,0,0); (0,0,1);(0,0,2);(0,1,0);(0,1,1);(0,1,2);(0,2,0);(0,2,1);
                   (0,2,2);(1,0,0);(1,0,1);(1,0,2);(1,1,0);(1,1,2);(1,2,0);(1,2,1);
                   (1,2,2);(2,0,0);(2,0,1);(2,0,2);(2,1,0);(2,1,1);(2,1,2);(2,2,0);
                   (2,2,1);(2,2,2)] in
  List.filter (fun x -> let temp = (Hashtbl.find (st.cells) x) in temp.player = None) positions
*)
(* let children nd clist st =
  let generated_tree = game_tree_generate nd st in
  for i = 0 to (List.length clist)-1 do
    List.map (fun i -> )
  done

(*[heuristic_fxn valid_node] calculates the utility value based on a yet to be
  determined heuristic function from [valid_node]'s perspective in the game
*)
  let heuristic_fxn nd =
    failwith "unimplemented"

(* [alpha_beta_pruned_tree tree] returns a [tree] with the removed game states
   that don't necessarily need to be checked to find the optimal move. utility
   values are determined based on the heuristic function
*)
let alpha_beta_pruned_tree t =
  failwith "unimplemented"

(* [player_hint tree] returns the optimal [cell] that should be played by the
   current player to maximize chances of winning as the player
*)
let player_hint t =
  failwith "unimplemented"

(* [computer_turn tree] returns the optimal [cell] that should be played by the
   computer to maximize chances of winning as the computer
*)
let computer_turn t =
  failwith "unimplemented" *)
