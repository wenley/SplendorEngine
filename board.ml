open Globals
open Data

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

let card_at (tier:int) (index:int) (board:board) : card option =
  let rec item_at (n:int) (items:'a list) : 'a option =
    match n, items with
    | (_, []) -> None
    | (0, hd::tl) -> Some hd
    | (n, hd::tl) -> item_at (n-1) tl
  in
  let nth_card (index:int) (tier:tier) : card option =
    let { revealed=cards } = tier in
    item_at index cards
  in
  let maybe_tier : tier option = match tier with
    | 1 -> let { one=tier } = board in Some tier
    | 2 -> let { two=tier } = board in Some tier
    | 3 -> let { three=tier } = board in Some tier
    | _ -> None
  in
  Globals.Option.flat_map (nth_card index) maybe_tier

let string_of_tier (tier:tier) : string =
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

