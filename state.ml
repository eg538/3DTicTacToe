open Command
open Grid_3d
open Grid_2d 
open Parse_init

type state = {
  result  : player; (*Will contain the player who won*)
  cells   : ((int*int*int), cell) Hashtbl.t;
  current_player  : player;
  curr_score_1 : int;
  curr_score_2 : int;
  mode  : num_players;
  level : level;
  p1_avatar : player;
  p1_num_hints : int;
  p1_num_tries : int;
  p2_num_hints : int;
  p2_num_tries : int;
}

let num_helper inf =
  match inf.info_level with
  |Easy -> 7
  |Medium -> 5
  |Hard -> 3

let init_state inf = {
  result = None;
  current_player = Python;
  curr_score_1 = 0;
  curr_score_2 = 0;
  mode = inf.info_mode;
  level = inf.info_level;
  p1_avatar = inf.info_p1_avatar;
  p1_num_hints = num_helper inf;
  p1_num_tries = num_helper inf;
  p2_num_hints = num_helper inf;
  p2_num_tries = num_helper inf;
  cells = failwith "Unimplemented"
  }

let p1_score s = s.curr_score_1

let p2_score s = s.curr_score_2

let curr_player s = s.curr_player

let num_hints s =
  match s.current_player with
  | p1_avatar -> s.p1_num_hints
  | _ -> s.p2_num_hints

let num_tries s = let check = s.p1_avatar in
  match s.current_player with
  | check -> s.p1_num_tries
  | _ -> s.p2_num_tries

let get_result s = s.result

let get_result_message s =
  match s.mode with
  | Single -> begin
    match s.result with
    | Caml -> "Caml wins!"
    | Python -> "Python wins!"
    | None -> "No one won"
  end
  | Multi -> if s.result = s.p1_avatar then
              "Congratulations! You won the Java cup!"
            else if s.result <> None then
              "Sad! You didn't win the Java cup, but try again next time for that steaming mug of Java!"
            else
              "Oh no! You were close to winning the Java cup!"

let rec find_cell s (pl, x, y) = failwith "Unimplemented"

let make_move s (pl, x, y) plyr = let old_val = Hashtbl.find s.cells (pl, x, y) in
                                  Hashtbl.replace s.cells (pl, x, y) ({old_val with player = plyr})









let hint = failwith "Unimplemented"

let board s = s.cells

let avatars s =
  match s.p1_avatar with
  | Caml -> [("player1", Caml); ("player2", Python)]
  | Python -> [("player1", Python); ("player2", Caml)]
  | None -> []

let play_move st (pl, row, col) = 
  let cell_interest = Hashtbl.find st (pl, row, col) in 
  Hashtbl.replace st (pl, row, col) {cell_interest with player = st.current_player};
    let plane0 = Hashtbl.fold (fun (pln, r, c) v acc -> if pl = 0 then v::acc else acc) st [] in
    let plane1 = Hashtbl.fold (fun (pln, r, c) v acc -> if pl = 1 then v::acc else acc) st [] in
    let plane2 = Hashtbl.fold (fun (pln, r, c) v acc -> if pl = 2 then v::acc else acc) st [] in
    let place_cell = Hashtbl.find st (pl, row, col) in
    if win_evaluation place_cell plane0 plane1 plane2 then 
      let p1_check = st.p1_avatar in
      match st.current_player with
      | p1_check -> {st with curr_score_1 = st.curr_score_1 + 1}
      | _ -> {st with curr_score_2 = st.curr_score_2 + 1}
    else 
      st

let do' c st =
  match c with
  | Play str -> parse_init_file str |> init_state
  | Score -> st
  | Quit -> st
  | Restart -> st
  | Try (pl, row, col) -> let p1_check = st.p1_avatar in
    begin
    match s.current_player with
    | p1_check -> {s with p1_num_tries = if s.p1_num_tries > 0 then s.p1_num_tries - 1 else 0}
    | _ -> {s with p2_num_tries = if s.p2_num_tries > 0 then s.p2_num_tries - 1 else 0}
    end
  | Place (pl, row, col) -> play_move st (pl, row, col)
  | Hint -> let p1_check = st.p1_avatar in
    begin
    match s.current_player with
    | p1_check -> {s with p1_num_hints = if s.p1_num_hints > 0 then s.p1_num_hints - 1 else 0}
    | _ -> {s with p2_num_hints = if s.p2_num_hints > 0 then s.p2_num_hints - 1 else 0}
    end
  | Look -> st
  | Turns -> st
  | _ -> raise InvalidCommand
