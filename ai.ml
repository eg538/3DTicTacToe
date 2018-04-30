open State

type valid_node = {board: cell list -> cell list -> cell list; mutable move: int*int*int}
(*type tree = Leaf | Node of valid_node * tree * tree (*this is a binary tree, not necessarily the case?*)
                             (*hashtbl, streams & laziness *) *)
type 'a stream = Cons of 'a * (unit -> 'a stream)
type nodeStream = Cons of int * (unit -> nodeStream)

let rec game_tree_generate move st =
  let remaining_cells = available_cells st in
  Cons (move, fun () ->
      if List.mem move remaining_cells then
        (game_tree_generate move st) else )
(*[available_cells st] is the list of cells that are vacant in the 3D game board
 * that st contains*)
let available_cells st =
  let positions = [(0,0,0); (0,0,1);(0,0,2);(0,1,0);(0,1,1);(0,1,2);(0,2,0);(0,2,1);
                   (0,2,2);(1,0,0);(1,0,1);(1,0,2);(1,1,0);(1,1,2);(1,2,0);(1,2,1);
                   (1,2,2);(2,0,0);(2,0,1);(2,0,2);(2,1,0);(2,1,1);(2,1,2);(2,2,0);
                   (2,2,1);(2,2,2)] in
  List.filter (fun x -> let temp = (Hashtbl.find (st.cells) x) in temp.player = None) positions

let children nd clist st =
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
  failwith "unimplemented"
