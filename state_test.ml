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
  "init_krazy_happ" >:: (fun _ -> assert_equal false st.krazy_happ);
  "init_krazy_bomb" >:: (fun _ -> assert_equal false st.krazy_bomb_happ);
  "init_moves_made" >:: (fun _ -> assert_equal 0 st.moves_made);

  "add_icon_p" >:: (fun _ -> assert_equal p (Hashtbl.find st0.tttBoard (0,0,0)));
  "add_icon_c" >:: (fun _ -> assert_equal c (Hashtbl.find st0.tttBoard (1,2,0)));
  "empty_cell" >:: (fun _ -> assert_equal n (Hashtbl.find st0.tttBoard (2,2,2)));
]
(*2d diagonal*)
let st1 = init_state "single python easy normal"
let st1 = do' (Place (0,0,0)) st1
let st1 = do' (Place (2,2,2)) st1
let st1 = do' (Place (0,1,1)) st1
let st1 = do' (Place (0,2,0)) st1
let st1 = do' (Place (0,2,2)) st1
let win1 = [[(0,2,2);(0,1,1);(0,0,0)]]

(*3d column *)
let st2 = init_state "single python easy normal"
let st2 = do' (Place (0,0,0)) st2
let st2 = do' (Place (0,1,2)) st2
let st2 = do' (Place (1,0,0)) st2
let st2 = do' (Place (0,2,2)) st2
let st2 = do' (Place (2,0,0)) st2
let win2 = [[(1,0,0);(0,0,0);(2,0,0)]]

(*2d horizontal*)
let st3 = init_state "single python easy normal"
let st3 = do' (Place (0,0,0)) st3
let st3 = do' (Place (1,2,0)) st3
let st3 = do' (Place (2,2,2)) st3
let st3 = do' (Place (1,2,1)) st3
let st3 = do' (Place (2,2,1)) st3
let st3 = do' (Place (1,2,2)) st3
let win3 = [[(1,2,0);(1,2,2);(1,2,1)]]

(*2d vertical*)
let st4 = init_state "single python easy normal"
let st4 = do' (Place (1,0,0)) st4
let st4 = do' (Place (2,0,0)) st4
let st4 = do' (Place (0,0,0)) st4
let st4 = do' (Place (2,1,0)) st4
let st4 = do' (Place (2,2,2)) st4
let st4 = do' (Place (2,2,0)) st4
let win4 = [[(2,1,0);(2,2,0);(2,0,0)]]

(*3d diagonal #1 *)
let st5 = init_state "single python easy normal"
let st5 = do' (Place (0,0,0)) st5
let st5 = do' (Place (2,0,0)) st5
let st5 = do' (Place (1,1,0)) st5
let st5 = do' (Place (2,2,2)) st5
let st5 = do' (Place (2,2,0)) st5
let win5 = [[(2,2,0);(0,0,0);(1,1,0)]]

let st6 = init_state "single python easy normal"
let st6 = do' (Place (2,0,0)) st6
let st6 = do' (Place (2,1,0)) st6
let st6 = do' (Place (1,1,0)) st6
let st6 = do' (Place (1,1,2)) st6
let st6 = do' (Place (0,2,0)) st6
let win6 = [[(0,2,0);(2,0,0);(1,1,0)]]

let st6' = init_state "single python easy normal"
let st6' = do' (Place (2,0,0)) st6'
let st6' = do' (Place (2,1,0)) st6'
let st6' = do' (Place (1,1,0)) st6'
let st6' = do' (Place (1,1,2)) st6'
let st6' = do' (Place (0,2,0)) st6'
let st6' = do' (Place (2,0,1)) st6'
let st6' = do' (Place (0,0,0)) st6'
let st6' = do' (Place (0,0,2)) st6'
let st6' = do' (Place (2,2,0)) st6'
let win6' = [[(2,2,0);(0,0,0);(1,1,0)]]

let st7 = init_state "single python easy normal"
let st7 = do' (Place (0,0,0)) st7
let st7 = do' (Place (0,1,0)) st7
let st7 = do' (Place (0,0,2)) st7
let st7 = do' (Place (1,1,2)) st7
let st7 = do' (Place (1,0,1)) st7
let st7 = do' (Place (2,1,0)) st7
let st7 = do' (Place (2,0,0)) st7
let st7 = do' (Place (2,1,1)) st7
let st7 = do' (Place (2,0,2)) st7
let st7 = do' (Place (2,1,2)) st7
let win7 = [[(2,1,0);(2,1,1);(2,1,2)]]

(*can most_recent_win contain multiple lists*)
let st8 = init_state "single python easy normal"
let st8 = do' (Place (0,0,0)) st8
let st8 = do' (Place (0,0,2)) st8
let st8 = do' (Place (0,1,0)) st8
let st8 = do' (Place (0,1,2)) st8
let st8 = do' (Place (0,2,0)) st8
let st8 = do' (Place (0,2,2)) st8
let st8 = do' (Place (1,0,0)) st8
let st8 = do' (Place (1,0,2)) st8
let st8 = do' (Place (1,1,0)) st8
let st8 = do' (Place (1,1,2)) st8
let st8 = do' (Place (1,2,0)) st8
let st8 = do' (Place (1,2,2)) st8
let st8 = do' (Place (2,0,0)) st8
let win8 = [[(1,0,0);(0,0,0);(2,0,0)];[(0,2,0);(2,0,0);(1,1,0)]]

(*can most_recent_win contain multiple lists*)
let st8' = init_state "single python easy normal"
let st8' = do' (Place (0,0,0)) st8'
let st8' = do' (Place (0,0,2)) st8'
let st8' = do' (Place (0,1,0)) st8'
let st8' = do' (Place (0,1,2)) st8'
let st8' = do' (Place (0,2,0)) st8'
let st8' = do' (Place (0,2,2)) st8'
let st8' = do' (Place (1,0,0)) st8'
let st8' = do' (Place (1,0,2)) st8'
let st8' = do' (Place (1,1,0)) st8'
let st8' = do' (Place (1,1,2)) st8'
let st8' = do' (Place (1,2,0)) st8'
let st8' = do' (Place (1,2,2)) st8'
let st8' = do' (Place (2,0,0)) st8'
let st8' = do' (Place (2,0,2)) st8'
let st8' = do' (Place (2,1,0)) st8'
let st8' = do' (Place (2,1,2)) st8'
let st8' = do' (Place (2,2,0)) st8'
let st8' = do' (Place (2,2,2)) st8'
let win8' = [[(2,2,2);(2,0,2);(2,1,2)];
             [(0,2,2);(1,2,2);(2,2,2)];
             [(2,2,2);(1,1,2);(0,0,2)]]

(*make sure that (1,1,1) is inactive*)
let st9a = init_state "single python easy normal"
let st9a = do' (Place (0,0,0)) st9a
let st9a = do' (Place (1,2,0)) st9a
let st9a = do' (Place (2,2,2)) st9a
let st9b = init_state "single python easy normal"
let st9b = do' (Place (1,1,0)) st9b
let st9b = do' (Place (1,0,1)) st9b
let st9b = do' (Place (1,1,2)) st9b
let st9c = init_state "single python easy normal"
let st9c = do' (Place (1,0,1)) st9c
let st9c = do' (Place (1,1,0)) st9c
let st9c = do' (Place (1,2,1)) st9c

let st10 = init_state "single python easy normal"
let st10 = do' (Place (0,0,0)) st10
let st10 = do' (Place (1,0,0)) st10
let st10 = do' (Place (0,0,1)) st10
let st10 = do' (Place (1,0,1)) st10
let st10 = do' (Place (0,0,2)) st10
let st10 = do' (Place (1,0,2)) st10
let st10 = do' (Place (0,1,0)) st10
let st10 = do' (Place (1,1,2)) st10
let st10 = do' (Place (0,1,2)) st10
let st10 = do' (Place (1,1,0)) st10
let st10 = do' (Place (0,2,0)) st10
let st10 = do' (Place (2,1,0)) st10
let st10 = do' (Place (0,2,2)) st10
let st10 = do' (Place (2,1,2)) st10
let st10 = do' (Place (0,2,1)) st10
let st10 = do' (Place (2,2,1)) st10
let st10 = do' (Place (0,1,1)) st10
let win10 = [[(0,2,0);(0,1,1);(0,0,2)];
             [(0,2,2);(0,1,1);(0,0,0)];
             [(0,1,0);(0,1,2);(0,1,1)];
             [(0,0,1);(0,2,1);(0,1,1)]]

(*single python easy normal*)
let tests_wins = [
  (*simple 2d diagonal win *)
  "2d_diag_win_p1" >:: (fun _ -> assert_equal 1 (st1.curr_score_1));
  "2d_diag_win_p2" >:: (fun _ -> assert_equal 0 (st1.curr_score_2));
  "2d_diag_recent_win" >:: (fun _ -> assert_equal win1 st1.most_recent_win);

  (*simple 3d column win *)
  "3d_col_win_p1" >:: (fun _ -> assert_equal 1 (st2.curr_score_1));
  "3d_col_win_p2" >:: (fun _ -> assert_equal 0 (st2.curr_score_2));
  "3d_col_recent_win" >:: (fun _ -> assert_equal win2 st2.most_recent_win);

  (*simple 2d horizontal win *)
  "2d_horz_win_p1" >:: (fun _ -> assert_equal 0 (st3.curr_score_1));
  "2d_horz_win_p2" >:: (fun _ -> assert_equal 1 (st3.curr_score_2));
  "2d_horz_recent_win" >:: (fun _ -> assert_equal win3 st3.most_recent_win);

  (*simple 2d vertical win *)
  "2d_vert_win_p1" >:: (fun _ -> assert_equal 0 (st4.curr_score_1));
  "2d_vert_win_p2" >:: (fun _ -> assert_equal 1 (st4.curr_score_2));
  "2d_vert_recent_win" >:: (fun _ -> assert_equal win4 st4.most_recent_win);

  (*3d diagonal win #1*)
  "3d_diag_win1_p1" >:: (fun _ -> assert_equal 1 (st5.curr_score_1));
  "3d_diag_win1_p2" >:: (fun _ -> assert_equal 0 (st5.curr_score_2));
  "3d_diag1_recent_win" >:: (fun _ -> assert_equal win5 st5.most_recent_win);

  (*3d diagonal win #2*)
  "3d_diag_win2_p1" >:: (fun _ -> assert_equal 1 (st6.curr_score_1));
  "3d_diag_win2_p2" >:: (fun _ -> assert_equal 0 (st6.curr_score_2));
  "3d_diag2_recent_win" >:: (fun _ -> assert_equal win6 st6.most_recent_win);

  (*double 3d vertical group diagonal win *)
  "3d_doub_vert_group_p1" >:: (fun _ -> assert_equal 2 (st6'.curr_score_1));
  "3d_doub_vert_group_p2" >:: (fun _ -> assert_equal 0 (st6'.curr_score_2));
  "3d_doub_vert_group_recent_win" >:: (fun _ -> assert_equal win6' st6'.most_recent_win);

  (*double 3d horizontal group diagonal win*)
  "3d_doub_horz_group_p1" >:: (fun _ -> assert_equal 2 (st7.curr_score_1));
  "3d_doub_horz_group_p2" >:: (fun _ -> assert_equal 1 (st7.curr_score_2));
  "3d_doub_horz_group_recent_win" >:: (fun _ -> assert_equal win7 st7.most_recent_win);

  (*one vertical slice + multiple most_recent_wins*)
  "3d_vertical_slice_p1a" >:: (fun _ -> assert_equal 4 (st8.curr_score_1));
  "3d_vertical_slice_p2a" >:: (fun _ -> assert_equal 2 (st8.curr_score_2));
  "3d_vertical_slice_recent_wina" >:: (fun _ -> assert_equal win8 st8.most_recent_win);

  (*full vertical slice covered*)
  "3d_vertical_slice_p1b" >:: (fun _ -> assert_equal 8 (st8'.curr_score_1));
  "3d_vertical_slice_p2b" >:: (fun _ -> assert_equal 8 (st8'.curr_score_2));
  "3d_vertical_slice_recent_winb" >:: (fun _ -> assert_equal win8' st8'.most_recent_win);

  (*(1,1,1) tests inactive *)
  "inactive_111_horz_p1" >:: (fun _ -> assert_equal 0 (st9a.curr_score_1));
  "inactive_111_horz_p2" >:: (fun _ -> assert_equal 0 (st9a.curr_score_2));

  "inactive_111_vert_p1" >:: (fun _ -> assert_equal 0 (st9a.curr_score_1));
  "inactive_111_vert_p2" >:: (fun _ -> assert_equal 0 (st9a.curr_score_2));

  "inactive_111_3d_diag_p1" >:: (fun _ -> assert_equal 0 (st9a.curr_score_1));
  "inactive_111_3d_diag_p2" >:: (fun _ -> assert_equal 0 (st9a.curr_score_2));

  (*one horizontal slice (localized 2d grid instance)*)
  "2d_horz_plane_p1" >:: (fun _ -> assert_equal 8 (st10.curr_score_1));
  "2d_horz_plane_p2" >:: (fun _ -> assert_equal 1 (st10.curr_score_2));
  "2d_horz_plane_recent_win" >:: (fun _ -> assert_equal win10 st10.most_recent_win);

  
]

let game_play_edge_cases = []

let suite =
  "Adventure test suite"
  >::: List.flatten [init_tests;tests_wins]

let _ = run_test_tt_main suite
