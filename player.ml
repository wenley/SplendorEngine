open Data;;

type player = {
  name : string;
  tokens : tokens;
  nobles : noble list;
  cards : card list;
  reserved: card list;
}

let empty_player (name:string) : player =
  { name; tokens=TokenMap.empty; nobles=[]; cards=[]; reserved=[] }

let reserve_card_at (index:int) (player:player) : card option =
  try Some (List.nth player.reserved index)
  with Invalid_argument(_) -> None

(* Should only be used during a purchase action,
 * in tandem with balancing tokens and adding the card back *)
let remove_reserved (index:int) (player:player) : (card option * player) =
  let card = reserve_card_at index player in
  match card with
  | None -> (None, player)
  | Some card ->
      let rec remove_card_at (index:int) (cards:card list) : card list =
        match index, cards with
        | _, [] -> []
        | 0, hd :: tl -> tl
        | n, hd :: tl -> hd :: remove_card_at (n-1) tl
      in
      let new_reserved = remove_card_at index player.reserved in
      (Some card, { player with reserved = new_reserved })

let total_discount ({ cards }:player) : cost =
  let add_card (cost:cost) (card:card) : cost =
    let color = card.color in
    let has_key = ColorMap.exists (fun k _ -> k = color) cost in
    match has_key with
    | false -> ColorMap.add color 1 cost
    | true ->
        let old_value = ColorMap.find color cost in
        ColorMap.add color (old_value + 1) cost
  in
  List.fold_left add_card ColorMap.empty cards

let score_for_card (card:card) = card.score
let score_for_noble (noble:noble) = noble.score

let total_score (score:'a -> int) (items:'a list) : int =
  let scores = List.map score items in
  List.fold_left (+) 0 scores

let score_for_player (player:player) : int =
  let card_score = total_score score_for_card player.cards in
  let noble_score = total_score score_for_noble player.nobles in
  card_score + noble_score

let string_of_player (player:player) : string =
  let name_string = "Player " ^ player.name in
  let score_string = "Total Score: " ^ (string_of_int (score_for_player player)) in
  let token_string = "Tokens: " ^ (verbose_string_of_tokens player.tokens) in
  let discount_string = "Card Discounts: " ^ (verbose_string_of_cost (total_discount player)) in
  let reserve_string =
    "Reserved Cards: " ^
    (Globals.string_of_list verbose_string_of_card "\n" player.reserved)
  in
  let lines = name_string :: score_string :: token_string :: discount_string :: reserve_string :: [] in
  Globals.string_of_list (fun s -> s) "\n" lines

let print_player (player:player) : unit =
  print_string (string_of_player player)

