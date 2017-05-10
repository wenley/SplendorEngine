open Globals
open Data

(* Note: this could be genericized into 'a list *)
type 'a deck = {
  deck: 'a list;
  revealed: 'a list
}

type tier_level = One | Two | Three

let reveal_one (tier:'a deck) : 'a deck =
  let { deck=deck; revealed=revealed } = tier in
  match deck with
  | [] -> tier
  | card :: deck -> { deck=deck; revealed=card::revealed }

let rec reveal (tier:'a deck) (n:int) : 'a deck =
  if n <= 0 then tier
  else
    let tier = reveal_one tier in
    reveal tier (n - 1)

let take_card (tier:card deck) (index:int) : (card * card deck) =
  let { deck=deck; revealed=revealed } = tier in
  let taken = List.nth revealed index in
  let shown_after_take = List.filter ((<>) taken) revealed in
  let tier_before_reveal = { deck=deck; revealed=shown_after_take } in
  (taken, reveal_one tier_before_reveal)

type board = {
  one: card deck;
  two: card deck;
  three: card deck;
  nobles: noble list;
  tokens: tokens
}

let card_at (level:tier_level) (index:int) (board:board) : card option =
  let rec item_at (n:int) (items:'a list) : 'a option =
    match n, items with
    | (_, []) -> None
    | (0, hd::tl) -> Some hd
    | (n, hd::tl) -> item_at (n-1) tl
  in
  let nth_card (index:int) (tier:card deck) : card option =
    let { revealed=cards } = tier in
    item_at index cards
  in
  let tier = match level with
    | One -> let { one=tier } = board in tier
    | Two -> let { two=tier } = board in tier
    | Three -> let { three=tier } = board in tier
  in
  nth_card index tier

let purchase (level:tier_level) (index:int) (board:board) : (card option * board) =
  match card_at level index board with
  | None -> (None, board)
  | Some card -> (None, board)

let string_of_tier (tier:card deck) : string =
  let { deck=deck; revealed=revealed } = tier in
  let deck_depth = List.length deck in
  let board_2 = string_of_list verbose_string_of_card "\n" revealed in
  Printf.sprintf "(%d cards remaining)%s" deck_depth board_2

let string_of_board (board:board) : string =
  let { one=one; two=two; three=three; nobles=nobles; tokens=tokens } = board in
  let one_string = string_of_tier one in
  let two_string = string_of_tier two in
  let three_string = string_of_tier three in
  let noble_string = string_of_list string_of_noble "\n" nobles in
  Printf.sprintf "Tier 1: %s\nTier 2: %s\nTier 3: %s\nNobles: %s" one_string two_string three_string noble_string
;;

