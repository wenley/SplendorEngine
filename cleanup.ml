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

