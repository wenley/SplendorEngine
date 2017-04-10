
module Option = struct
  let map f value =
    match value with
    | None -> None
    | Some x -> Some (f x)
end;;

let rec maybe_find pred (collection : 'a list) : 'a option =
  match collection with
  | [] -> None
  | el :: tail ->
      if pred el then Some el
      else maybe_find pred tail
;;

module Data = struct
  type color = Blue | Black | Green | Red | White
  type score = int

  type token = Normal of color | Gold
  type tokens = (token * int) list

  type cost = (color * int) list
  type noble = score * cost
  type card = score * color * cost

  (* Purchased cards + Reserved cards + Nobles acquired + Tokens held *)
  type player = card list * card list * noble list * tokens

  (* Tier 1 + Tier 2 + Tier 3 + Unclaimed nobles *)
  type board = card list * card list * card list * tokens * noble list

  type state = board * player

  let total_score (player : player) : int =
    let cards, _, nobles, _ = player in
    let card_score : int =
      List.fold_left (fun total (score, _, _) -> total + score) 0 cards
    in
    let noble_score : int =
      List.fold_left (fun total (score, _) -> total + score) 0 nobles
    in
    card_score + noble_score

end;;

module Prompt = struct
  let prompt_players () =
    []
end;;

module Display = struct
  let print_player player : unit =
    ()

  let print_board board : unit =
    ()
end;;

