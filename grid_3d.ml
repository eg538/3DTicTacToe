open Parse_init

type cell = {cell: (int*int*int); player: player}

type winType3D =
  | WinV of cell list
  | WinH of cell list
  | WinNone

type board = ((int*int*int), cell) Hashtbl.t

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

let rec asciiBoard_helper (a, b, c) s acc = try (match (a, b, c) with
                                        | (3, 0, 0) -> acc
                                        | (1, 1, 1) -> asciiBoard_helper (a, b, c + 1) s (acc ^ "|---|")
                                        | _ -> let vl = Hashtbl.find s (a, b, c) in
                                        begin let spot =
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

let board_list_of_cells b = Hashtbl.fold (fun k v acc -> v::acc) b []

let get_plane (pl, _ , _ ) = pl

let rec get_parent_plane cell_pos lst_of_cells =
  let pl_match = get_plane cell_pos.cell in
  List.filter (fun x -> (get_plane x.cell) = pl_match ) lst_of_cells

let cells_left b =
  let lst = board_list_of_cells b in
  List.filter (fun x -> x.player = None) lst

let is_taken cell_pos b =
  let lst = board_list_of_cells b in
  List.exists (fun x -> ((x.cell = cell_pos) && (x.player = None)) ) lst

let cell_valid cell =
  match cell.cell with
  | (pl, x, y) -> if pl = 1 && x = 1 && y = 1 then false
    else if x < 0 || x >=3 then false
    else if y < 0 || y >=3 then false
    else true

(* fix !!*)
let move_valid cell b =
  (cell_valid cell) && (is_taken cell.cell b)

let get_x (_, ex, _) = ex

let get_y (_, _, why) = why

let fst' (y,_,_) = y

let snd' (_,y,_) = y

let thd (_,_,y) = y

let diagonal_hardcode c lst_of_cells = (*for corner cells *)
  match c.cell with 
  | p,x,y when (x=y) -> (List.filter (fun i -> (i.cell |> thd = (c.cell |> thd)) && (i.cell |> snd' = (c.cell |> snd')) && (p = fst' c.cell)) lst_of_cells) @ [c] 
  | p,x,y when (x=0 && y=2) -> (List.filter (fun i -> ((i.cell |> thd = 1)&&(i.cell |> snd' = 1)) && ((i.cell |> thd = 2)&&(i.cell |> snd' = 0)) && (p = fst' c.cell)) lst_of_cells) @ [c] 
  | p,x,y when (x=2 && y=0) -> (List.filter (fun i -> ((i.cell |> thd = 1)&&(i.cell |> snd' = 1)) && ((i.cell |> thd = 0)&&(i.cell |> snd' = 2) && (p = fst' c.cell))) lst_of_cells) @ [c]  
  | _ -> failwith "non-exhaustive match "


let three_row_2d_cells c lst_of_cells = 
  match c.cell with 
  | (p,x,y) when (x=1 && y <> 1) || (x<>1 && y =1) -> (*edge cells but not corners*)
    (List.filter (fun i -> (thd (c.cell) = thd (i.cell)) 
    && (p = fst' c.cell)) lst_of_cells)::(List.filter (fun i -> (snd' (c.cell) = snd' (i.cell)) 
    && (p = fst' c.cell)) lst_of_cells)::[] (*horizontal 3-in-row, vertical 3-in-row*)
  | (p,x,y) when (x<>1 && y<>1) -> 
    (List.filter (fun i -> (thd (c.cell) = thd (i.cell)) 
    && (p = fst' c.cell)) lst_of_cells)::(List.filter (fun i -> (snd' (c.cell) = snd' (i.cell))
    && (p = fst' c.cell)) lst_of_cells)::(diagonal_hardcode c lst_of_cells)::[] (*horizontal 3-in-row, vertical 3-in-row, *)
  | (p,_,_) -> 
    (List.filter (fun i -> (thd (c.cell) = thd (i.cell))
    &&(p = fst' c.cell)) lst_of_cells)::(List.filter (fun i -> (snd' (c.cell) = snd' (i.cell))
    && (p = fst' c.cell)) lst_of_cells)::(diagonal_hardcode c lst_of_cells)::[] (*center cell in a plane*) 

let three_row_2d cell lst_of_cells =
  let cell_player = cell.player in
  let cell_ex = get_x cell.cell in
  let cell_why = get_y cell.cell in
  let cell_pl = get_plane cell.cell in
  (* three in a column *)
  ((List.mem {cell = (cell_pl, cell_ex, cell_why -1); player = cell_player} (get_parent_plane cell lst_of_cells)) &&
    (List.mem {cell = (cell_pl, cell_ex, cell_why + 1); player = cell_player} (get_parent_plane cell lst_of_cells)))

  ||

  (* three in a row *)
  ((List.mem {cell = (cell_pl, cell_ex -1, cell_why); player = cell_player} (get_parent_plane cell lst_of_cells)) &&
    (List.mem {cell = (cell_pl, cell_ex + 1, cell_why); player = cell_player} (get_parent_plane cell lst_of_cells)))

  ||

  (* three in a diagonal starting from leftmost corner *)
  ((List.mem {cell = (cell_pl, cell_ex -1, cell_why -1); player = cell_player} (get_parent_plane cell lst_of_cells)) &&
    (List.mem {cell = (cell_pl, cell_ex + 1, cell_why + 1); player = cell_player} (get_parent_plane cell lst_of_cells )))

  ||

  (* three in a diagonal starting from rightmost corner *)
  ((List.mem {cell = (cell_pl, cell_ex -1, cell_why + 1); player = cell_player} (get_parent_plane cell lst_of_cells)) &&
    (List.mem {cell = (cell_pl, cell_ex + 1, cell_why -1); player = cell_player} (get_parent_plane cell lst_of_cells)))


let place (pl, row, col) b plyr =
  let c = get_cell (pl, row, col) b in
  if move_valid c b then
    Hashtbl.replace b (pl, row, col) {c with player = plyr}
  else
    raise (Failure "InvalidCell")

(*relocated thd, fst', and snd' methods *)

    (*grid_space = state.cells *)
let horizontal_3d_group c b =
  let grid_space = board_list_of_cells b in
  match c.cell with
    | (0,y,z) when (y=z) -> List.filter (fun a -> (a.cell |> snd')=(a.cell |> thd)) grid_space (*cell list of length 2*)
    | 0,_,_ -> List.filter (*cell list of length 2*)
              (fun a -> a.cell <> (0,0,1) && a.cell <> (0,1,0) && a.cell <> (0,1,2) && a.cell <> (0,2,1)) grid_space
    | 1,y,z when (y=z) -> List.filter (fun a -> (a.cell |> snd')=(a.cell |> thd)) grid_space
    | 1,_,_ -> List.filter
              (fun a -> a.cell <> (1,0,1) && a.cell <> (1,1,0) && a.cell <> (1,1,2) && a.cell <> (1,2,1)) grid_space
    | 2,y,z when (y=z) -> List.filter (fun a -> (a.cell |> snd')=(a.cell |> thd)) grid_space
    | _ -> List.filter
              (fun a -> a.cell <> (2,0,1) && a.cell <> (2,1,0) && a.cell <> (2,1,2) && a.cell <> (2,2,1)) grid_space

let vertical_3d_groups c b =
  let grid_space = board_list_of_cells b in
    match c.cell with
    | x,0,z when (x=z) -> List.filter (fun a -> (a.cell |> fst')=(a.cell |> thd)) grid_space
    | 0,_,_ -> List.filter
              (fun a -> a.cell <> (0,0,1) && a.cell <> (1,0,0) && a.cell <> (2,0,1) && a.cell <> (1,0,2)) grid_space
    | x,1,z when (x=z) -> List.filter (fun a -> (a.cell |> fst')=(a.cell |> thd)) grid_space
    | 1,_,_ -> List.filter
                  (fun a -> a.cell <> (0,0,1) && a.cell <> (1,1,0) && a.cell <> (1,1,2) && a.cell <> (2,1,1)) grid_space
    | x,2,z when (x=z) -> List.filter (fun a -> (a.cell |> fst')=(a.cell |> thd)) grid_space
    | _ -> List.filter
              (fun a -> a.cell <> (0,2,1) && a.cell <> (1,2,0) && a.cell <> (2,2,1) && a.cell <> (1,2,2)) grid_space

let diag_check c b =
  let diag_h = horizontal_3d_group c b in
  let diag_v = vertical_3d_groups c b in
  let verdict_h = (List.for_all (fun x -> x.player = c.player) diag_h) in (*checks whether 2 of 3-in-row instance is true*)
  let verdict_v = (List.for_all (fun x -> x.player = c.player) diag_v) in
  match (verdict_h, verdict_v) with
  | true, true -> WinH diag_h, WinV diag_v
  | true, false -> WinH diag_h, WinNone
  | false, true -> WinNone, WinV diag_v
  | false, false -> WinNone, WinNone

let find_vertical_cells c b =
  let grid_space = board_list_of_cells b in
  let s = c.cell |> snd' in
  let t = c.cell |> thd in
  List.filter (fun i -> (i.cell |> snd')=s && (i.cell |> thd)=t) grid_space

let col_check c b =
  let cell_1 = (find_vertical_cells c b) |> List.hd in
  let cell_2 = (find_vertical_cells c b) |> List.rev |> List.hd in
  (cell_1.player = c.player) && (cell_2.player = c.player)

let win_evaluation c b =
  let p1 = Hashtbl.fold (fun (pln, r, c) v acc -> if pln = 0 then v::acc else acc) b [] in
  let p2 = Hashtbl.fold (fun (pln, r, c) v acc -> if pln = 1 then v::acc else acc) b [] in
  let p3 = Hashtbl.fold (fun (pln, r, c) v acc -> if pln = 2 then v::acc else acc) b [] in (*3 grids, 3 cell lists *)
  let diag_check_truth = (((diag_check c b )|> fst) <> WinNone) || (((diag_check c b) |> snd) <> WinNone) in
  let cases_3d = (diag_check_truth) || (col_check c b) in
  let case_1 = three_row_2d c p1 in
  let case_2 = three_row_2d c p2 in (*checking all horizontal cases*)
  let case_3 = three_row_2d c p3 in
  match get_plane c.cell with
  | 0 -> case_1 || cases_3d
  | 1 -> case_2 || cases_3d
  | 2 -> case_3 || cases_3d
  | _ -> failwith "impossible"

let cells_occupied b = (*is each plane represented as a cell list *)
  let lst_cells = board_list_of_cells b in
  let whole_space = List.fold_left (fun a x -> x::a) [] lst_cells in
  List.filter (fun cell -> cell.player <> None) whole_space

let get_the_win c current_player b=
  if (win_evaluation c b) then
    let diag_check_truth = (((diag_check c b )|> fst) <> WinNone) && (((diag_check c b) |> snd) <> WinNone) in
    match (diag_check_truth) with
    | true  -> begin
        match diag_check c b with
        | WinNone, WinNone -> []
        | WinH x, WinV y -> x@y
        | WinH x, WinNone -> x
        | WinNone, WinV y -> y
        | _, _ -> [] (*for pattern-non-exhaustive case*)
      end
    | _ -> find_vertical_cells c b
  else []
