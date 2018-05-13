open State
open Grid_3d
open Parse_init
open Types

(* type tree = Leaf of int*int*int | Node of board * (tree list) *)
type node = {move: int*int*int; available: cell list; taken: cell list; h_score: int}
type gsTree = Leaf | Node of node * (gsTree list)
type three_count = {none: int; curr_p: int; opp: int}

let rec three_count move clst plyr acc =
  match clst with
  | [] -> acc
  | h::t ->
    let new_acc = if player_at_cell h = None then
          {acc with none= acc.none + 1}
        else if player_at_cell h <> plyr then
          {acc with curr_p = acc.curr_p + 1}
        else
          {acc with opp = acc.opp + 1}
    in
    three_count move t plyr new_acc

let move_heur_fn_helper (move: cell) (clst: cell list) (plyr: player) (acc: int) =
  let counts = three_count move clst plyr {none = 0; curr_p = 0; opp = 0} in
  (* print_endline ("None: " ^ (string_of_int counts.none));
  print_endline ("Curr_p: " ^ (string_of_int counts.curr_p));
  print_endline ("Opp: " ^ (string_of_int counts.opp)); *)
  if counts.curr_p = 2 then
    3
  else if counts.opp = 2 then
    2
  else if (counts.curr_p = 1 && counts.none = 2) then
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

let game_tree_generate st d s_thresh = let b = board st in
  let p = other_player (curr_player st) in
    gt_gen_help b (cells_left b) (-1, -1, -1) (min_int) p d s_thresh

let rec tree_size t accum =
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
  Place mve

let rec med_ai_move_helper st rem_cells thresh =
  let game_tree = game_tree_generate st 5 thresh in
  (* print_endline "medium level ai calculating move..."; *)
  (* print_endline (string_of_int (List.length rem_cells)); *)
  if List.length rem_cells = 1 then
    (* (print_endline "One cell left"; *)
    Place ((List.hd rem_cells).cell)
  else
    (print_endline "AYYY";
    match game_tree with
    | Leaf -> easy_ai_move st
    | Node (nd, children) -> if List.length children = 0 then
        med_ai_move_helper st rem_cells (thresh -1)
      else
      begin
      print_endline "POOP";
      match dfs children game_tree nd.h_score with
      | Leaf -> easy_ai_move st
      | Node (mve, _) ->
        Place mve.move
      end)

let medium_ai_move st =
  print_endline "HERE";
  let rem_cells = cells_left (board st) in
  (* let thresh = (-1) * ( 30.0 /. (List.length rem_cells |> float_of_int) -. 0.5 |> int_of_float) in *)
  (* let depth = 30.0 /. (List.length rem_cells |> float_of_int) +. 0.5 |> int_of_float in *)
  (* let thresh = if List.length rem_cells <= 3 then min_int else 0 in *)
  med_ai_move_helper st rem_cells 0


let get_node t = match t with
    | Leaf -> failwith "Invalid input"
    | Node (info, _) -> info

let rec maximin_helper minf (ndlist: node list) (st: state) (d: int) (a: int) (b: int) (valu: int) =
  match ndlist with
  | [] -> valu
  | h::t -> let copy_st = {st with tttBoard = copy (board st)} in
    let val' = minf h (do' (Place h.move) copy_st) (d-1) a (min b valu) in
      if val' <= a then
        valu
      else if val' < valu then
        maximin_helper minf t st d a b val'
      else
        maximin_helper minf t st d a b valu

let rec maximin_AB f (nd : node) (st : state) (d : int) (a: int) (b: int) =
  let brd = board st in
  let rem = cells_left brd in
  if List.length rem = 0 || d = 0 then
    nd.h_score
  else
    let valu = max_int in
    let tree = game_tree_generate st 1 (min_int) in
    match tree with
    | Leaf -> nd.h_score
    | Node (info, children) ->
        maximin_helper f (List.map get_node children) st d a b valu

let rec minimax_help f (ndlist : node list) (st : state) (d : int) (a: int) (b: int) (valu: int)=
  match ndlist with
  | [] -> valu
  | h::t -> let copy_st = {st with tttBoard = copy (board st)} in
    let val' = maximin_AB f h (do' (Place h.move) copy_st) (d - 1) (max a valu) b in
      if val' >= b then
        valu
      else if val' > valu then
        minimax_help f t st d a b val'
      else
      minimax_help f t st d a b valu

let rec minimax_AB (nd : node) (st : state) (d : int) (a: int) (b: int) =
  let brd = board st in
  let rem = cells_left brd in
  if (List.length rem) = 0 || d = 0 then
    nd.h_score
  else
    let valu = min_int in
    let tree = game_tree_generate st 1 (min_int) in
    match tree with
    | Leaf -> nd.h_score
    | Node (info, children) ->
      minimax_help minimax_AB (List.map get_node children) st d a b valu

let rec minimax_move_helper st h_val children =
  match children with
  | [] -> easy_ai_move st
  | (Node (mve, _))::t -> if mve.h_score = h_val then
      Place mve.move
    else minimax_move_helper st h_val t

let rec minimax_move st h_val tree =
  match tree with
  | Leaf -> easy_ai_move st
  | Node (info, children) -> if info.h_score = h_val then
      Place info.move
    else
      minimax_move_helper st h_val children

let rec find_helper thresh lst =
  match lst with
  | [] -> (-1, -1, -1)
  | h::t -> if h.h_score >= thresh then h.move
    else
      find_helper thresh t

let find_greater thresh tree =
  match tree with
  | Leaf -> failwith "Invalid argument"
  | Node (info, children) -> find_helper thresh (List.map get_node children)

let hard_ai_move st =
  let tree = game_tree_generate st 1 (min_int) in
  let try_find = find_greater 5 tree in
  if try_find = (-1, -1, -1) then
    let rem_cells = cells_left (board st) in
    let depth = 30.0 /. (List.length rem_cells |> float_of_int) +. 0.5 |> int_of_float in
    print_endline (string_of_int depth);
    let move_h_val = minimax_AB (get_node tree) st depth min_int max_int in
    minimax_move st move_h_val tree
  else
    Place try_find

let player_hint st = let lvl = game_level st in
  match lvl with
  | Easy -> hard_ai_move st
  | Medium -> medium_ai_move st
  | Hard -> medium_ai_move st
