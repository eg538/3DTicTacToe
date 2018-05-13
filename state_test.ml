open OUnit2
open Types
open Command
open Grid_3d
open State
open Ai
open Parse_init
open ANSITerminal
open Gui
open Graphics

let empty_b = empty_board
let st = init_state "single python easy normal"

let st0 = init_state "single python easy normal"
let st0 = do' (Place (0,0,0)) st0
let st0 = do' (Place (1,2,0)) st0

let p = {cell=(0,0,0);player=Python}
let c = {cell=(1,2,0);player=Caml}
let n = {cell = (2,2,2);player=None}


(*single python easy normal*)
let init_tests = [
  "init_board" >:: (fun _ -> assert_equal empty_b (st.tttBoard));
  "init_curr_plyr" >:: (fun _ -> assert_equal Python st.current_player);
  "init_curr_score1" >:: (fun _ -> assert_equal 0 st.curr_score_1);
  "init_curr_score2" >:: (fun _ -> assert_equal 0 st.curr_score_2);
  "init_num_plyrs" >:: (fun _ -> assert_equal Single st.num_players);
  "init_mode" >:: (fun _ -> assert_equal Normal st.mode);
  "init_level" >:: (fun _ -> assert_equal Easy st.level);
  "init_p1_avatar" >:: (fun _ -> assert_equal Python st.p1_avatar);
  "init_p1_num_hints" >:: (fun _ -> assert_equal 7 st.p1_num_hints);
  "init_p1_num_tries" >:: (fun _ -> assert_equal 7 st.p1_num_tries);
  "init_p2_num_hints" >:: (fun _ -> assert_equal 7 st.p2_num_hints);
  "init_p2_num_tries" >:: (fun _ -> assert_equal 7 st.p2_num_tries);
  "init_most_recent_win" >:: (fun _ -> assert_equal [] st.most_recent_win);
  "init_winner" >:: (fun _ -> assert_equal None st.winner);
  "init_game_end" >:: (fun _ -> assert_equal false st.game_end);
  "init_k_bomb" >:: (fun _ -> assert_equal false st.k_bomb);
  "init_k_disappearing_sqs" >:: (fun _ -> assert_equal {cell=(0,0,0);player=None} st.k_disappearing_sqs);

  "add_icon_p" >:: (fun _ -> assert_equal p (Hashtbl.find st0.tttBoard (0,0,0)));
  "add_icon_c" >:: (fun _ -> assert_equal c (Hashtbl.find st0.tttBoard (1,2,0)));
  "empty_cell" >:: (fun _ -> assert_equal n (Hashtbl.find st0.tttBoard (2,2,2)));
]

let tests2 = [
  "init_board" >:: (fun _ -> assert_equal empty_board (st.tttBoard));
]

let suite =
  "Adventure test suite"
  >::: init_tests

let _ = run_test_tt_main suite
