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
  let tier1_deck = reveal { deck=tier1_cards; revealed=[] } 4 in
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
;;

(* Testing code *)
let board = start_board 4 in
print_string (string_of_board board) ; print_newline ()
