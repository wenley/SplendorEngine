
(**
 * Steps:
 * - Start the game
 *   - Get player count
 *   - Initialize board data (start.ml)
 *   - Get player names
 *   - Make game object
 * - Game loop, for each player
 *   - Display state of the world
 *   - Read action
 *   - Validate action against board + player state
 *     - Translate from relative indexes to concrete data (cards)
 *     - Read another action if invalid
 *   - Perform action
 *     - Update all relevant data structures
 *   - Check for cleanup
 *     - Claim nobles
 *     - Put back tokens
 * - After each full loop, check for end of game
 *)

type game = {
  players: Player.player list;
  board: Board.board;
}

let rec prompt_player_count () : int =
  print_string "How many players? (2 to 4 supported) " ;
  let input = read_line () in
  match input with
  | "2" -> 2
  | "3" -> 3
  | "4" -> 4
  | _ ->
      Printf.printf "Unprocessable entry %s\n" input;
      prompt_player_count ()

let rec prompt_player_names (n:int) (players:Player.player list) : Player.player list =
  if n <= 0 then players
  else
    (Printf.printf "%d players remaining. Enter a player name: " n;
    let name = read_line () in
    let player = Player.empty_player name in
    prompt_player_names (n-1) (player :: players))

let start_game () : game =
  let player_count = prompt_player_count () in
  let board = Start.start_board player_count in
  let players = prompt_player_names player_count [] in
  { players; board }
;;

let game = start_game ();;
