
(**
 * Steps:
 * - Start the game (start.ml)
 *   - Get player count
 *   - Initialize board data
 *   - Get player names
 *   - Make game object
 * - Game loop, for each player (engine.ml)
 *   - Display state of the world (display.ml)
 *   - Read action (prompt.ml)
 *   - Validate action against board + player state (validation.ml)
 *     - Translate from relative indexes to concrete data (cards)
 *     - Read another action if invalid
 *   - Perform action (process.ml)
 *     - Update all relevant data structures
 *   - Check for cleanup (cleanup.ml)
 *     - Claim nobles
 *     - Put back tokens
 * - After each full loop, check for end of game (game.ml, engine.ml)
 *)

module Engine (G:Game.Game) : Engine = struct
  (**
   * Given a player and a board state, let the player take a turn and
   * return the updated player and board states.
   *)
  let rec player_turn (player:Player.player) (board:Board.board) : (Player.player * Board.board) =
    Display.fancy_print_board board;
    Player.print_player player;
    let action = Prompt.read_action () in
    match Validation.valid action player board with
    | false ->
        (Printf.printf "Invalid action given game state.\n";
         player_turn player board)
    | true ->
        let player, board = Process.process_action action player board in
        let player, board = Cleanup.claim_nobles player board in
        let player = Cleanup.discard_tokens player in
        (player, board)

  (**
   * Given the current state of the game with all players having taken the same
   * number of turns, allow all players to take one more round of turns.
   *)
  let play_round (game:Game.game) : Game.game =
    (**
     * Players are independent; therefore, can map over "old" values, update at end
     * Board state needs to be propogated; therefore, fold to collect
     *)
    let process_turn acc player : (Player.player list * Game.game) =
      let new_players, game = acc in
      let new_player, new_board = player_turn player game.Game.board in
      let new_game = { game with Game.board=new_board } in
      ((new_players @ new_player :: []), new_game)
    in
    let new_players, new_game = List.fold_left process_turn ([], game) game.Game.players in
    { new_game with Game.players=new_players }

  (**
   * Given a starting game state, allow all players to take turns until the game
   * in finished.
   *)
  let rec play (game:Game.game) : Game.game =
    match G.game_over game with
    | true -> game
    | false -> play (play_round game)
end

module RealEngine = Engine(Game.DefaultGame)
