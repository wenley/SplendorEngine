open Globals
open Data

type player = {
  name: string;
  tokens: tokens;
  reserved: card list;
  purchased: card list;
  nobles: noble list
}

(* Note: this could be genericized into 'a list *)
type tier = {
  deck: card list;
  revealed: card list
}

let reveal_one (tier:tier) : tier =
  let { deck=deck; revealed=revealed } = tier in
  match deck with
  | [] -> tier
  | card :: deck -> { deck=deck; revealed=card::revealed }

let rec reveal (tier:tier) (n:int) : tier =
  if n <= 0 then tier
  else
    let tier = reveal_one tier in
    reveal tier (n - 1)

let take_card (tier:tier) (index:int) : (card * tier) =
  let { deck=deck; revealed=revealed } = tier in
  let taken = List.nth revealed index in
  let shown_after_take = List.filter ((<>) taken) revealed in
  let tier_before_reveal = { deck=deck; revealed=shown_after_take } in
  (taken, reveal_one tier_before_reveal)

type board = {
  one: tier;
  two: tier;
  three: tier;
  nobles: noble list;
  tokens: tokens
}

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
  let all_nobles = Shuffle.shuffle (Read.read_nobles "nobles.txt") in
  Lists.first all_nobles (num_players + 1)

let start_board (num_players : int) : board =
  let tokens = start_tokens num_players in
  let nobles = start_nobles num_players in
  let tier1_cards = Shuffle.shuffle (Read.read_deck "tier1.txt") in
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

let string_of_tier (tier:tier) : string =
  let { deck=deck; revealed=revealed } = tier in
  let deck_depth = List.length deck in
  let board_2 = string_of_list verbose_string_of_card "\n" revealed in
  Printf.sprintf "Cards Remaining: %d\nBoard:%s" deck_depth board_2

let string_of_board (board:board) : string =
  let { one=one; two=two; three=three; nobles=nobles; tokens=tokens } = board in
  let one_string = string_of_tier one in
  let noble_string = string_of_list string_of_noble "\n" nobles in
  Printf.sprintf "Tier 1: %s\nNobles: %s" one_string noble_string

;;

(* Testing code *)
let board = start_board 4 in
print_string (string_of_board board) ; print_newline ()
