open Data;;
open Board;;

let start_tokens (num_players:int) : int TokenMap.t =
  let color_token_count =
    match num_players with
    | 2 -> 4
    | 3 -> 5
    | 4 -> 7
    | _ -> failwith (Printf.sprintf "Unsupported number of players %d" num_players)
  in
  TokenMap.empty |> TokenMap.add (Normal Black) color_token_count
                 |> TokenMap.add (Normal Blue) color_token_count
                 |> TokenMap.add (Normal Green) color_token_count
                 |> TokenMap.add (Normal Red) color_token_count
                 |> TokenMap.add (Normal White) color_token_count
                 |> TokenMap.add Gold 5

let start_nobles (num_players : int) : noble list =
  let all_nobles = Shuffle.shuffle (Read.read_nobles "game_data/nobles.txt") in
  Globals.Lists.first all_nobles (num_players + 1)

let start_board (num_players : int) : board =
  let tokens = start_tokens num_players in
  let nobles = start_nobles num_players in
  let tier1_cards = Shuffle.shuffle (Read.read_deck "game_data/tier1.txt") in
  let tier1_deck = reveal 4 { deck=tier1_cards; revealed=[] } in
  {
    one = tier1_deck;
    two = {
      deck = []; revealed = []
    };
    three = {
      deck = []; revealed = []
    };
    nobles = nobles;
    tokens = tokens
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

let start_game () : Game.game =
  let player_count = prompt_player_count () in
  let board = start_board player_count in
  let players = prompt_player_names player_count [] in
  { Game.players; Game.board }
;;

let game = start_game () in
let { Game.board } = game in
print_string (string_of_board board) ; print_newline ()
