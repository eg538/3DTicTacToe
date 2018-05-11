open State
open Grid_3d
open Parse_init
open Types

(* type tree = Leaf of int*int*int | Node of board * (tree list) *)
type node = {move: int*int*int; available: cell list; taken: cell list; h_score: int}
type gsTree = Leaf | Node of node * (gsTree list)
type three_count = {none: int; curr_p: int; opp: int}

(* let string_of_cell mv = let mve = cell_coords mv in
  ((string_of_int (fst' mve))^ "," ^ (string_of_int (snd' mve))^ ","^(string_of_int (thd mve)))

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "Python"
  | Caml -> "Caml"
  | None -> "None" *)

let rec three_count move clst plyr acc =
  match clst with 
  | [] -> acc
  | h::t -> 
    let new_acc = if player_at_cell h = plyr then
        {acc with curr_p = acc.curr_p + 1}
        else
        begin
        match player_at_cell h with
        | None -> {acc with none = acc.none + 1}
        | _ -> {acc with opp = acc.opp + 1}
        end 
    in
    three_count move t plyr new_acc

let move_heur_fn_helper (move: cell) (clst: cell list) (plyr: player) (acc: int) =
  let counts = three_count move clst plyr {none = 0; curr_p = 0; opp = 0} in
  (* print_endline ("None:" ^ (string_of_int counts.none));
  print_endline ("Player: "^(string_of_int counts.curr_p));
  print_endline ("Opp:" ^ (string_of_int counts.opp)); *)
  if counts.curr_p = 3 then 
    3
  else if counts.opp = 2 || counts.curr_p = 2 then
    2
  else if counts.none = 2 then
    1
  else
    -1

(* let rec move_heur_fn_helper (move: cell) (clst: cell list) (plyr: player) (acc: int) =
  match clst with 
  | [] -> acc
  | h::t -> if cell_coords h <> cell_coords move then
      begin
      match player_at_cell h with
      | plyr -> move_heur_fn_helper move t plyr (acc + 2)
      | None -> move_heur_fn_helper move t plyr (acc + 1)
      | _ -> move_heur_fn_helper move t plyr (acc + 0)
      end
    else
    move_heur_fn_helper move t plyr acc *)

let rec move_heur_fn move b clstlst plyr acc = 
  match clstlst with
  | [] -> acc
  | h::t -> let score = move_heur_fn_helper move h plyr 0 in
    move_heur_fn move b t plyr (acc + score)

let rec placement_helper f b remaining_cells plyr d acc s_thresh =
  match remaining_cells with
  | [] -> acc
  | h::t -> let cpy = copy b in (*grabs possible move from remaining cells*)
    let all_threes = all_three_in_row_cells h b in (*grabs all cells that could create three-in-a-row with cell*)
    let score = move_heur_fn h b all_threes plyr 0 in (*scores the move*)
    place (cell_coords h) cpy plyr; (*places the move on the copied board*)
    if score >= s_thresh then (*if score is above certain threshold, create node and its children*)
      let mv = cell_coords h in
      placement_helper f b t plyr d ((f cpy t mv score plyr d (s_thresh + 2))::acc) s_thresh
    else (*don't create node and its children*)
      placement_helper f b t plyr d acc s_thresh
    
let rec gt_gen_help b rem_cells mv scr plyr d s_thresh=
  let occupied_cells = cells_occupied b in
  let nd = {move = mv; available = rem_cells; taken = occupied_cells; h_score = scr} in
  if d <> 0 then
    let p = other_player plyr in 
    Node (nd, placement_helper gt_gen_help b rem_cells p (d - 1) [] s_thresh)
  else
    Node (nd, [Leaf])

let game_tree_generate st d s_thresh= let b = board st in
  let p = curr_player st in
    gt_gen_help b (cells_left b) (-1, -1, -1) (min_int) p d s_thresh

let rec tree_size t accum= 
  match t with 
  | Leaf -> accum
  | Node (nd, children) -> accum + 1 + List.fold_left (fun acc a -> acc + tree_size a accum) 0 children

let rec move_h_score t accum = 
  match t with 
  | Leaf -> accum
  | Node (nd, children) -> accum + nd.h_score + List.fold_left (fun acc a -> acc + move_h_score a accum) 0 children

let rec dfs child_lst mv acc = 
  match child_lst with 
  | [] -> mv
  | h::t -> let sc = move_h_score h 0 in
      if sc > acc then
        dfs t h sc
      else
        dfs t mv acc

let rec easy_ai_move_help clst num acc= 
  if num > 0 then 
    match clst with 
    | [] -> acc
    | h::t -> easy_ai_move_help t (num - 1) (cell_coords h)
  else
    acc

let easy_ai_move st = 
  let b = board st in
  let rem = cells_left b in
  let mve = easy_ai_move_help rem (Random.int (List.length rem)) (List.hd rem |> cell_coords) in 
  Play ((string_of_int (fst' mve))^ "," ^ (string_of_int (snd' mve))^ ","^(string_of_int (thd mve)))

let medium_ai_move st = 
  let game_tree = game_tree_generate st 5 0 in
  match game_tree with 
  | Leaf -> print_endline "Using random ai"; easy_ai_move st
  | Node (nd, children) -> 
    begin
    match dfs children game_tree nd.h_score with 
    | Leaf -> easy_ai_move st
    | Node (mve, _) ->
      Play ((string_of_int (fst' mve.move))^","^(string_of_int (snd' mve.move))^","^(string_of_int (thd mve.move)))
    end
  
let maximin_helper f (ndlist : node list) (st : state) (d : int) (a: int) (b: int) = 
  failwith "Unimplemented"

let maximin_AB f (nd : node) (st : state) (d : int) (a: int) (b: int) = failwith "Unimplemented"

let rec minimax_help f (ndlist : node list) (st : state) (d : int) (a: int) (b: int) (val: int)= 
  match ndlist with 
  | [] -> val
  | h::t -> let val' = maximin_AB f h st (d - 1) (max a val) b in
      if val' >= b then
        val
      else
        minimax_help f t st d a b val'

let rec minimax_AB (nd : node) (st : state) (d : int) (a: int) (b: int) = 
  let b = board st in
  let rem = cells_left b in
  if (List.length rem) = 0 || d = 0 then
    nd.h_score
  else 
    let val = min_int in
    let tree = game_tree_generate st 1 (min_int) in
    match tree with 
    | Leaf -> nd.h_score
    | Node (info, children) -> 
      minimax_help minimax_AB children st d a b val


 
let hard_ai_move st = failwith "Unimplemented"

let player_hint st plyr = failwith "Unimplemented"

(* let rec print_head t =
  match t with
  | Leaf (_, _, _) -> print_endline ""
  | Node (b, children) -> print_string ("*****************\n"^(asciiBoard b)^"*****************\n")

let rec print_children t =
  match t with
  | Leaf (_, _, _) -> print_endline ""
  | Node (b, c) -> List.iter (fun a -> print_head a) c *)

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
