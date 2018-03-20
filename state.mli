type state

(*[init_state info] is the initial state of the game
 * ass determined by info (TODO: decide what type info will be),
 * which is composed of information (mode, avatar, level), where
 *  - mode: single or multiplayer mode
 *  - avatar: the player(s)'s avatar(s)
 *  - level: easy, medium or difficult level
 *)
val init_state: info -> state

(*[p1score st] is player 1's score when the game is in state [st].
 * Player 1 is the human player*)
val p1score: state -> int

(*[p2score st] is player 2's score when the game is in state [st].
 * Player 2 could either be a second human player or the computer*)
val p2score: state -> int

(*[curr_player st] is the player whose turn it is to go when game is in
 * state [st]*)
val curr_player: state -> string

(*[asciiBoard st] is the ascii string representation of the 3D tictactoe
 * board when the game is in state [st]
 * NOTE: for testing before implementation of GUI*)
val asciiBoard: state -> string

(*[hint st] is the optimal move for the current player of [st] when game is
 * in state [st]*)
val hint: state -> string

(*[board st] is information regarding which spots in the board are filled and
 * with which player's move
 * TODO: figure out what type this function should return*)
val board: state -> string

(*[avatars st] is the assignment of the avatars when game is in state in the form
 * [("player1", player1's avatar); ("player2", player2's avatar)*)
val avatars: state -> string * plep list

(*[do' c st] is [st'] if executing command [c] in state [st] results
 * in [st']. The following describe the valid commands and the result
 * that [do'] should return for each one when applied to a state [st]
 *  - The “start” (and its alternate "play"), “try”, “place”
 *    and “restart” return an appropriately updated [st'] as described
 *    in the charter, if their object is valid in [st]. If not, then [st']
 *    is equivalent to [st]
 *      + The object of "start" (and "play") are valid if state is NULL
 *        and player has entered valid information for mode of play, avatar
 *        choices, and level.
 *      + The object of "try" and "place" are valid if state is not NULL and
 *        the move is to place at a empty spot in the board
 *      + The object of "restart" is always valid if state is not NULL. If "restart"
 *        is valid, then calling [do'] on command "restart" and any state [st]
 *        returns NULL
 *  - The “quit” command us always possible. This command, along with the
 *    commands "look", “hint” and “score”, leave the observable state
 *    unchanged ([st'] is equivalent to [st]).
 *      + The object "hint" is only valid if there remains an empty spot on
 *        the board and the game
 *      + The object "score" is only valid if it requests the score of a valid player
 *        (NOTE: This is so that the current player will not only be able to see his own
 *        score but also the score of the opponent, whether human or computer)
 *      + The object "look" is only valid before the proper GUI is implemented and for use
 *        with the text GUI (NOTE: Do we need look if we get to the fancier GUI, since then
 *        the board is always in display?)
 *    has not terminated (TODO: how to determine if the game should terminate?)
 * TODO: what is the behavior of do if [c] is not a valid command?
 * effects: none. do' shouldn't print or display anything in order to maintain
 * model-view-controller
 *)
val do': Command.command -> state -> state