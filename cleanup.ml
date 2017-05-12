open Data;;
open Player;;
open Board;;

let noble_satisfied (cards:cost) (noble:noble) : bool =
  let cards_enough (color:color) (required:int) : bool =
    let has_key = ColorMap.exists (fun c -> fun _ -> c = color) cards in
    match required, has_key with
    | 0, _ -> true
    | _, false -> false
    | n, true ->
        let card_count = ColorMap.find color cards in
        card_count >= required
  in
  ColorMap.for_all cards_enough noble.cost

let claim_nobles (player:player) (board:board) : (player * board) =
  let discounts = total_discount player in
  let claimed, unclaimed = List.partition (noble_satisfied discounts) board.nobles in
  ({ player with nobles = List.append player.nobles claimed },
   { board with nobles = unclaimed })

let rec discard_token (player:player) : player =
  let add_token token _ string : string = string_of_token token ^ string in
  let tokens_available = TokenMap.fold add_token player.tokens "" in
  Printf.printf "You have tokens: %s\n" (verbose_string_of_tokens player.tokens);
  Printf.printf "Choose a token to discard: [%s] " tokens_available;
  let input = read_line () in
  match token_of_string input with
  | None ->
      Printf.printf "Unknown choice: %s\n" input;
      discard_token player
  | Some token ->
      let has_key = TokenMap.exists (fun t -> fun _ -> t = token) player.tokens in
      if has_key then
        use_token token player
      else
        (Printf.printf "You have no %s tokens\n" (string_of_token token);
        discard_token player)

let rec discard_tokens (player:player) : player =
  let add_token_counts _ count total = count + total in
  let token_count = TokenMap.fold add_token_counts player.tokens 0 in
  if token_count <= 10 then player
  else
    discard_tokens (discard_token player)

(* Testing code *)
;;

let tokens = TokenMap.empty |> TokenMap.add (Normal Black) 12 in
let player = { (empty_player "You") with tokens = tokens } in
discard_tokens player
;;