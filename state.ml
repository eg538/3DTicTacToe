open Command
open Grid_3d
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
  cells = let hash = Hashtbl.create 123456 in
          Hashtbl.add hash (0, 0, 0) {cell = (0, 0, 0); player = None};
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
  }

let p1_score s = s.curr_score_1
let p2_score s = s.curr_score_2
let curr_player s = s.curr_player
let p1_hints s = s.p1_num_hints
let p2_hints s = s.p2_num_hints
let p1_tries s = s.p1_num_tries
let p2_tries s = s.p2_num_tries
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
  

let rec find_cell s (pl, x, y) = Hashtbl.find s.cells (pl, x, y)

let make_move s (pl, x, y) plyr = let old_val = Hashtbl.find s.cells (pl, x, y) in 
                                  Hashtbl.replace s.cells (pl, x, y) ({old_val with player = plyr})

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

 

let asciiBoard s = asciiBoard_helper (0, 0, 0) s.cells ""

let print_board s = print_string (asciiBoard s)

let hint = failwith "Unimplemented"

let board s = s.cells

let avatars s = 
  match s.p1_avatar with 
  | Caml -> [("player1", Caml); ("player2", Python)]
  | Python -> [("player1", Python); ("player2", Caml)]
  | None -> []

let do' c st = 
  match c with 
  | Play -> init_state 
  | Score 
  | Quit 
  | Restart
  | Try (pl, row, col)
  | Place (pl, row, col)
  | Hint 
  | Look
  | Turns
  | Start
  | _ 

