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

let add_noble (noble:noble) (player:player) : player =
  let { nobles } = player in
  { player with nobles = noble :: nobles }

let add_card (card:card) (player:player) : player =
  let { cards } = player in
  { player with cards = card :: cards }

let add_reserved_card (card:card) (player:player) : player =
  let { reserved } = player in
  { player with reserved = card :: reserved }

let add_token (token:token) (player:player) : player =
  let { tokens } = player in
  { player with tokens=TokenCounter.increment token tokens }

let use_token (token:token) (player:player) : player =
  let { tokens } = player in
  { player with tokens=TokenCounter.decrement token tokens }

(* Should only be used during a purchase action,
 * in tandem with balancing tokens and adding the card back *)
let remove_reserved (index:int) (player:player) : player =
  let rec remove_card_at (index:int) (cards:card list) : card list =
    match index, cards with
    | _, [] -> []
    | 0, hd :: tl -> tl
    | n, hd :: tl -> hd :: remove_card_at (n-1) tl
  in
  let { reserved=reserved } = player in
  { player with reserved = remove_card_at index reserved }

let total_discount ({ cards }:player) : cost =
  let add_card (cost:cost) (card:card) : cost =
    let { score=_; color=color; cost=_ } = card in
    let has_key = ColorMap.exists (fun k _ -> k = color) cost in
    match has_key with
    | false -> ColorMap.add color 1 cost
    | true ->
        let old_value = ColorMap.find color cost in
        ColorMap.add color (old_value + 1) cost
  in
  List.fold_left add_card ColorMap.empty cards

let score_for_card ({ score }:card) = score
let score_for_noble ({ score }:noble) = score

let total_score (score:'a -> int) (items:'a list) : int =
  let scores = List.map score items in
  List.fold_left (+) 0 scores

let score_for_player ({ nobles; cards }:player) : int =
  let card_score = total_score score_for_card cards in
  let noble_score = total_score score_for_noble nobles in
  card_score + noble_score

let string_of_player (player:player) : string =
  let { name; tokens; nobles; cards; reserved } = player in
  let name_string = "Player " ^ name in
  let score_string = "Total Score: " ^ (string_of_int (score_for_player player)) in
  let token_string = "Tokens: " ^ (verbose_string_of_tokens tokens) in
  let discount_string = "Card Discounts: " ^ (verbose_string_of_cost (total_discount player)) in
  let reserve_string = "Reserved Cards: " ^ (Globals.string_of_list verbose_string_of_card "\n" reserved) in

  let lines = name_string :: score_string :: token_string :: discount_string :: reserve_string :: [] in
  Globals.string_of_list (fun s -> s) "\n" lines

let print_player (player:player) : unit =
  print_string (string_of_player player)

