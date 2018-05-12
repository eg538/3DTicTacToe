open Parse_init
open Types
(* type cell = {cell: (int*int*int); player: player}

type winType3D =
  | WinV of cell list list
  | WinH of cell list list
  | WinNone

type board = ((int*int*int), cell) Hashtbl.t *)

let empty_board =
let hash = Hashtbl.create 123456 in
Hashtbl.add hash (0, 0, 0) ({cell = (0, 0, 0); player = None});
Hashtbl.add hash (0, 0, 1) {cell = (0, 0, 1); player = None};
Hashtbl.add hash (0, 0, 2) {cell = (0, 0, 2); player = None};
Hashtbl.add hash (0, 1, 0) {cell = (0, 1, 0); player = None};
Hashtbl.add hash (0, 1, 1) {cell = (0, 1, 1); player = None};
Hashtbl.add hash (0, 1, 2) {cell = (0, 1, 2); player = None};
Hashtbl.add hash (0, 2, 0) {cell = (0, 2, 0); player = None};
Hashtbl.add hash (0, 2, 1) {cell = (0, 2, 1); player = None};
Hashtbl.add hash (0, 2, 2) {cell = (0, 2, 2); player = None};
Hashtbl.add hash (1, 0, 0) {cell = (1, 0, 0); player = None};
Hashtbl.add hash (1, 0, 1) {cell = (1, 0, 1); player = None};
Hashtbl.add hash (1, 0, 2) {cell = (1, 0, 2); player = None};
Hashtbl.add hash (1, 1, 0) {cell = (1, 1, 0); player = None};
Hashtbl.add hash (1, 1, 2) {cell = (1, 1, 2); player = None};
Hashtbl.add hash (1, 2, 0) {cell = (1, 2, 0); player = None};
Hashtbl.add hash (1, 2, 1) {cell = (1, 2, 1); player = None};
Hashtbl.add hash (1, 2, 2) {cell = (1, 2, 2); player = None};
Hashtbl.add hash (2, 0, 0) {cell = (2, 0, 0); player = None};
Hashtbl.add hash (2, 0, 1) {cell = (2, 0, 1); player = None};
Hashtbl.add hash (2, 0, 2) {cell = (2, 0, 2); player = None};
Hashtbl.add hash (2, 1, 0) {cell = (2, 1, 0); player = None};
Hashtbl.add hash (2, 1, 1) {cell = (2, 1, 1); player = None};
Hashtbl.add hash (2, 1, 2) {cell = (2, 1, 2); player = None};
Hashtbl.add hash (2, 2, 0) {cell = (2, 2, 0); player = None};
Hashtbl.add hash (2, 2, 1) {cell = (2, 2, 1); player = None};
Hashtbl.add hash (2, 2, 2) {cell = (2, 2, 2); player = None};
hash

let get_cell (pl, row, col) b = Hashtbl.find b (pl, row, col)

let cell_coords c = c.cell

(*[asciiBoard_helper (a,b,c) s acc] is a helper method for asciiBoard *)
let rec asciiBoard_helper (a, b, c) s acc =
  try (match (a, b, c) with
    | (3, 0, 0) -> acc
    | (1, 1, 1) -> asciiBoard_helper (a, b, c + 1) s (acc ^ "|---|")
    | _ -> let vl = Hashtbl.find s (a, b, c) in
      begin
        let spot =
            match vl.player with
            | Caml -> "| C |"
            | Python -> "| P |"
            | None -> "|   |" in
            if b = 2 && c = 2 then
              asciiBoard_helper (a + 1, 0, 0) s (acc ^ spot ^ "\n\n")
            else if c = 2 then
              asciiBoard_helper (a, b + 1, 0) s (acc ^ spot ^ "\n")
            else
              asciiBoard_helper (a, b, c + 1) s (acc ^ spot)
      end) with
    | _ -> "Not found: " ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ", " ^ (string_of_int c)

let asciiBoard b = asciiBoard_helper (0, 0, 0) b ""

let copy b = Hashtbl.copy b

(*[board_list_of_cells b] extracts all the cells from hashtable board [b] and
  puts all the cells in a list
*)
let board_list_of_cells b = Hashtbl.fold (fun k v acc -> v::acc) b []

(*[get_plane (pl,_,_)] returns the numeric value denoting which "level" in the
  3D tic-tac-toe space a certain coordinate ([pl],_,_) is in, i.e. top plane,
  middle plane, or bottom plane
*)
let get_plane (pl, _ , _ ) = pl

(*[get_parent_plane cell_pos lst_of_cells] finds all the cells from
  [lst_of_cells] that are part of the same level in the 3D grid space as
  [cell_pos]
*)
let rec get_parent_plane cell_pos lst_of_cells =
  let pl_match = get_plane cell_pos.cell in
  List.filter (fun x -> (get_plane x.cell) = pl_match ) lst_of_cells

let cells_left b =
  let lst = board_list_of_cells b in
  List.filter (fun x -> x.player = None) lst

(*[is_taken cell_pos b] checks to see whether the cell at coordinates [cell_pos]
  has been played by a player in the board [b]
*)
let is_taken cell_pos b =
  let lst = board_list_of_cells b in
  List.exists (fun x -> ((x.cell = cell_pos) && (x.player = None)) ) lst

(*[cell_valid cell] checks to see whether the coordinates of [cell] are valid*)
let cell_valid cell =
  match cell.cell with
  | (pl, x, y) -> if pl = 1 && x = 1 && y = 1 then false
    else if x < 0 || x >=3 then false
    else if y < 0 || y >=3 then false
    else true

(*[move_valid cell b] determines whether [cell] is an available move in board
  [b]
*)
let move_valid cell b =
  (cell_valid cell) && (is_taken cell.cell b)

let fst' (y,_,_) = y

let snd' (_,y,_) = y

let thd (_,_,y) = y

(*[diagonal_hardcode c lst_of_cells] finds the possible diagonal matches that
  [c] can be a part of in the same plane.
*)
let diagonal_hardcode c lst_of_cells =
  match c.cell with
  | p,x,y when (x=y) -> print_endline "case 1";
    begin
      let l1 = (List.filter (fun i -> ((i.cell |> thd) = (i.cell |> snd')) && (p = fst' c.cell)) lst_of_cells) in (*orig*)
      let l2 = (List.filter (fun i -> (((i.cell |> thd = 2) && (i.cell |> snd' = 0)) || ((i.cell |> snd' = 2)  && (i.cell |> thd = 0))) && (p = fst' c.cell)) lst_of_cells) in
      [l1;l2]
    end
  | p,x,y when (x=0 && y=2) || (x=2 && y=0) -> print_endline "case 2";
    let l1 = (List.filter (fun i -> (((i.cell |> thd = 1)&&(i.cell |> snd' = 1)) || ((i.cell |> thd = 0)&&(i.cell |> snd' = 2))) && (p = fst' c.cell)) lst_of_cells) @ [c] in
    let l2 = (List.filter (fun i -> (((i.cell |> thd = 1)&&(i.cell |> snd' = 1)) || ((i.cell |> thd = 2)&&(i.cell |> snd' = 0))) && (p = fst' c.cell)) lst_of_cells) @ [c] in
    [l1; l2]
  | _ -> failwith "non-exhaustive match "

(*[three_row_2d_cells c b] compiles a list of 3-in-a-row instances that are
  possible for a given cell [c] in board [b]; 3-in-a-row instances include
  vertical, horizontal, and diagonal matches.
*)
let three_row_2d_cells c b =
  let lst = board_list_of_cells b in
  let lst_of_cells = get_parent_plane c lst in
  match c.cell with
  | (p,x,y) when (x=1 && y <> 1) || (x<>1 && y =1) ->
    (List.filter (fun i -> (thd (c.cell) = thd (i.cell))
    && (p = fst' c.cell)) lst_of_cells)::(List.filter (fun i -> (snd' (c.cell) = snd' (i.cell))
    && (p = fst' c.cell)) lst_of_cells)::[]
  | (p,x,y) when (x<>1 && y<>1) ->
    let filt = (List.filter (fun i -> (thd (c.cell) = thd (i.cell))
                                      && (p = fst' c.cell)) lst_of_cells) in
    let filt2 = (List.filter (fun i -> (snd' (c.cell) = snd' (i.cell))
                                       && (p = fst' c.cell)) lst_of_cells) in
    let diag = diagonal_hardcode c lst_of_cells in
    let nth1 = List.nth diag 0 in
    let nth2 = List.nth diag 1 in
    filt::filt2::nth1::nth2::[]
  | (p,_,_) ->
    (List.filter (fun i -> (thd (c.cell) = thd (i.cell))
    &&(p = fst' c.cell)) lst_of_cells)::(List.filter (fun i -> (snd' (c.cell) = snd' (i.cell))
                                                               && (p = fst' c.cell)) lst_of_cells)::(List.nth (diagonal_hardcode c lst_of_cells) 0)::(List.nth (diagonal_hardcode c lst_of_cells) 1)::[]

(*[victory_on_plane c possible_instances] traverses through [possible_instances]
  to check whether at least one of [possible_instances] has resulted in a win
  for the player who has played [c]
*)
let rec victory_on_plane c possible_instances =
  match possible_instances with
  | [] -> false
  | h::t ->
    (List.for_all (fun m -> m.player = c.player) h) || (victory_on_plane c t)

let place (pl, row, col) b plyr =
  let c = get_cell (pl, row, col) b in
  if move_valid c b then
    Hashtbl.replace b (pl, row, col) {c with player = plyr}
  else
    raise (Failure "InvalidCell")

(*[horizontal_3d_group c b] extracts the diagonal instances that [c] is part of,
  by slicing the 3D grid space represented by hashtable board [b] horizontally
*)
let horizontal_3d_group c b =
  let grid_space = board_list_of_cells b in
  if (c.cell = (0,0,0) || (c.cell = (2,0,2))) then [(List.filter (fun a -> a.cell = (0,0,0) || a.cell = (1,0,1) || a.cell = (2,0,2)) grid_space)]
  else if (c.cell = (2,0,0)) || (c.cell = (0,0,2)) then [(List.filter (fun a -> a.cell = (2,0,0) || a.cell = (1,0,1) || a.cell = (0,0,2)) grid_space)]
  else if (c.cell = (0,2,0)) || (c.cell = (2,2,2)) then [(List.filter (fun a -> a.cell = (0,2,0) || a.cell = (1,2,1) || a.cell = (2,2,2)) grid_space)]
  else if ((c.cell = (2,2,0)) || (c.cell = (0,2,2))) then [(List.filter (fun a -> a.cell = (2,2,0) || a.cell = (1,2,1) || a.cell = (0,2,2)) grid_space)]
  else if (c.cell = (1,2,1)) then
    begin
      let l1 = (List.filter (fun a -> a.cell = (0,2,0) || a.cell = (1,2,1) || a.cell = (2,2,2)) grid_space) in
      let l2 = (List.filter (fun a -> (a.cell = (2,2,0)) || (a.cell = (1,2,1)) || (a.cell = (0,2,2))) grid_space) in
      [l1;l2]
    end
  else if (c.cell = (1,0,1)) then
    begin
      let l1 = (List.filter (fun a -> a.cell = (0,0,0) || a.cell = (1,0,1) || a.cell = (2,0,2)) grid_space) in
      let l2 = (List.filter (fun a -> a.cell = (2,0,0) || a.cell = (1,0,1) || a.cell = (0,0,2)) grid_space) in
      [l1;l2]
    end
  else []

  (*
  match c.cell with
  | (x,0,z) when (x=z) -> List.filter (fun a -> ((a.cell |> fst')=(a.cell |> thd)) && ((a.cell |> snd')=0)) grid_space (*cell list of length 2*)
  | _,0,_ -> List.filter (*cell list of length 2*)
               (fun a -> (a.cell = (2,0,0)) || (a.cell = (1,0,1)) || (a.cell = (0,0,2))) grid_space
  | x,1,z when (x=z) -> []
  | _,1,_ -> []
  | x,2,z when (x=z) -> List.filter (fun a -> ((a.cell |> fst')=(a.cell |> thd)) && (a.cell |> snd' = 2)) grid_space
  | _ -> List.filter
           (fun a -> a.cell = (2,2,0) || a.cell = (1,2,1) || a.cell = (0,2,2)) grid_space
*)
(*[vertical_3d_groups c b] extracts the diagonal instances that [c] is part of,
  by slicing the 3D grid space represented by hashtable board [b] horizontally
*)
let vertical_3d_groups c b =
  let grid_space = board_list_of_cells b in
  if (c.cell = (0,0,0) || c.cell = (2,2,0)) then [(List.filter (fun a -> a.cell = (0,0,0) || a.cell = (1,1,0) || a.cell = (2,2,0)) grid_space)]
  else if (c.cell = (0,2,0) || c.cell = (2,0,0)) then [(List.filter (fun a -> a.cell = (2,0,0) || a.cell = (1,1,0) || a.cell = (0,2,0)) grid_space)]
  else if (c.cell = (1,1,0)) then
    begin
      let l1 = (List.filter (fun a -> a.cell = (0,0,0) || a.cell = (1,1,0) || a.cell = (2,2,0)) grid_space) in
      let l2 = (List.filter (fun a -> a.cell = (2,0,0) || a.cell = (1,1,0) || a.cell = (0,2,0)) grid_space) in
      [l1;l2]
    end
  else if (c.cell = (0,0,2) || (c.cell = (2,2,2))) then [(List.filter (fun a -> a.cell = (0,0,2) || a.cell = (1,1,2) || a.cell = (2,2,2)) grid_space)]
  else if (c.cell = (2,0,2) || c.cell = (0,2,2)) then [List.filter (fun a -> a.cell = (2,0,2) || a.cell = (1,1,2) || a.cell = (0,2,2)) grid_space]
  else if (c.cell = (1,1,2)) then
    begin
      let l1 = (List.filter (fun a -> a.cell = (0,0,2) || a.cell = (1,1,2) || a.cell = (2,2,2)) grid_space) in
      let l2 = (List.filter (fun a -> a.cell = (2,0,2) || a.cell = (1,1,2) || a.cell = (0,2,2)) grid_space) in
      [l1;l2]
    end
  else []
(*match c.cell with
| 1,1,0 -> (List.filter (fun a -> a.cell = (0,0,0) || a.cell = (1,1,0) || a.cell = (2,2,0)) grid_space)::(List.filter
                        (fun a -> a.cell = (2,0,0) || a.cell = (1,1,0) || a.cell = (0,2,0)) grid_space)
| 1,1,2 -> (List.filter (fun a -> a.cell = (0,0,2) || a.cell = (1,1,2) || a.cell = (2,2,2)) grid_space)::(List.filter
                        (fun a -> a.cell = (2,0,2) || a.cell = (1,1,2) || a.cell = (0,2,2)) grid_space)
| x,0,z when (x=z) -> List.filter (fun a -> (a.cell |> fst')=(a.cell |> thd)) grid_space
| 0,_,_ -> List.filter
          (fun a -> a.cell <> (0,0,1) && a.cell <> (1,0,0) && a.cell <> (2,0,1) && a.cell <> (1,0,2)) grid_space
| x,1,z when (x=z) -> List.filter (fun a -> (a.cell |> fst')=(a.cell |> thd)) grid_space
| 1,_,_ -> List.filter
              (fun a -> a.cell <> (0,0,1) && a.cell <> (1,1,0) && a.cell <> (1,1,2) && a.cell <> (2,1,1)) grid_space
| x,2,z when (x=z) -> List.filter (fun a -> (a.cell |> fst')=(a.cell |> thd) && (a.cell |> snd' = 2)) grid_space
| _ -> List.filter
         (fun a -> a.cell <> (0,2,1) && a.cell <> (1,2,0) && a.cell <> (2,2,1) && a.cell <> (1,2,2) && (a.cell |> thd = 0)) grid_space
*)
(*[diag_check c b] finds the respective diagonal and horizontal wins that cover
  all three levels of the grid_space that relate to cell [c]
*)
let diag_check c b =
  let diag_h = horizontal_3d_group c b in
  let diag_v = vertical_3d_groups c b in
  let verdict_h = (match diag_h with
    | [] -> false
    | h::[] -> (List.for_all (fun x -> x.player = c.player) h)
    | h1::h2::[] -> (List.for_all (fun x -> x.player = c.player) h1) || (List.for_all (fun x -> x.player = c.player) h2)
    | _ -> false
    ) in
  let verdict_v = (match diag_v with
      | [] -> false
      | h::[] -> (List.for_all (fun x -> x.player = c.player) h)
      | h1::h2::[] -> (List.for_all (fun x -> x.player = c.player) h1) || (List.for_all (fun x -> x.player = c.player) h2)
      | _ -> false
  ) in
    (*(if diag_h <> [] then (List.for_all (fun x -> x.player = c.player) diag_h)
      else false) in *)
  (*let verdict_v = (List.for_all (fun x -> x.player = c.player) diag_v) in *)
  match (verdict_h, verdict_v) with
  | true, true -> WinH diag_h, WinV diag_v
  | true, false -> WinH diag_h, WinNone
  | false, true -> WinNone, WinV diag_v
  | false, false -> WinNone, WinNone

(*[find_vertical_cells c b] finds the other cells that are in the same column
  as [c]
*)
let find_vertical_cells c b =
  let grid_space = board_list_of_cells b in
  let s = c.cell |> snd' in
  let t = c.cell |> thd in
  List.filter
    (fun i -> (i.cell |> snd')=s && ((i.cell |> thd)=t) && (i<>c)) grid_space

(*[col_check c b] checks to see whether all the remaining cells in the column of
  [c] are taken by the same player that played [c] in [b]
*)
let col_check c b =
  if (((c.cell|> thd) = 1) && ((c.cell |> snd') = 1)) then false else
    begin
  let cell_1 = (find_vertical_cells c b) |> List.hd in
  let cell_2 = (find_vertical_cells c b) |> List.rev |> List.hd in
  (cell_1.player = c.player) && (cell_2.player = c.player)
  end

let player_at_cell c = c.player

let win_evaluation c b =
  let diag_check_truth =
    (((diag_check c b )|> fst) <> WinNone) || (((diag_check c b) |> snd) <> WinNone) in
  let cases_3d = (diag_check_truth) || (col_check c b) in
  let modified_3_row_2d_cells = List.filter (fun x -> List.length x <> 2) (three_row_2d_cells c b) in
  let case_1 = victory_on_plane c (modified_3_row_2d_cells) in
  let case_2 = victory_on_plane c (modified_3_row_2d_cells) in
  let case_3 = victory_on_plane c (modified_3_row_2d_cells) in
  match get_plane c.cell with
  | 0 -> case_1 || cases_3d
  | 1 -> case_2 || cases_3d
  | 2 -> case_3 || cases_3d
  | _ -> failwith "impossible"

let cells_occupied b =
  let lst_cells = board_list_of_cells b in
  (* let whole_space = List.fold_left (fun a x -> x::a) [] lst_cells in *)
  List.filter (fun cell -> cell.player <> None) lst_cells

let all_three_in_row_cells c b =
  let v = find_vertical_cells c b in
  let h = vertical_3d_groups c b in
  let h_3d = horizontal_3d_group c b in
  let plane_2d_inst = three_row_2d_cells c b in
  (v::h) @ h_3d @ plane_2d_inst

let get_the_win c current_player b=
  if (win_evaluation c b) then
    let diag_check_truth =
      (((diag_check c b )|> fst) <> WinNone) && (((diag_check c b) |> snd) <> WinNone) in
    match (diag_check_truth) with
    | true  -> begin
        match diag_check c b with
        | WinNone, WinNone -> []
        | WinH x, WinV y -> x @y
        | WinH x, WinNone -> x
        | WinNone, WinV y -> y
        | _, _ -> []
      end
    | _ -> [c ::(find_vertical_cells c b)](*[[c] @ (find_vertical_cells c b)] *)
  else []

let get_all_win_inst st c =
  let b = st.tttBoard in
  let col_3d = find_vertical_cells c b in (*cell list list*)
  let v_3d_diag = List.flatten (vertical_3d_groups c b) in (*cell list list*)
  let h_3d_diag = List.flatten (horizontal_3d_group c b) in (*cell list list*)
  let plane_2d_inst = List.flatten (three_row_2d_cells c b) in (*cell list list *)
  col_3d @ v_3d_diag @ h_3d_diag @ plane_2d_inst

let extract_cell_pos inst =
  List.map (fun x -> x.cell) inst
