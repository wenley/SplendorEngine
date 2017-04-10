open Data;;

module Validation = struct
  let color_counts (cards : Data.card list) : Data.cost =
    let rec add_color cost color =
      match cost with
      | [] -> [(color, 1)]
      | ((c, count) as head) :: tail ->
          if c = color then (color, count + 1) :: tail
          else head :: (add_color tail color)
    in 
    let colors = List.map (fun (_, color, _) -> color) cards in
    List.fold_left add_color [] colors

  let can_purchase (player : Data.player) (card : Data.card) : bool =
    let (_, _, cost) = card in
    let cost_after_cards : Data.cost =
      let subtract_cards ((color, count) : Data.color * int) : (Data.color * int) =
        let (cards, _, _, _) = player in
        let card_purchase_power = color_counts cards in
        match maybe_find (fun (c, i) -> c = color) card_purchase_power with
        | None -> (color, count)
        | Some (_, card_count) ->
            if card_count > count then (color, 0)
            else (color, count - card_count)
      in List.map subtract_cards cost
    in
    let cost_after_tokens : Data.cost =
      let subtract_tokens (color, count) : (Data.color * int) =
        let (_, _, _, tokens) = player in
        let hit (token, _) : bool =
          match token with
          | Data.Gold -> false
          | Data.Normal c -> c = color
        in
        match maybe_find hit tokens with
        | None -> (color, count)
        | Some (_, token_count) -> 
            if token_count >= count then (color, 0)
            else (color, count - token_count)
      in List.map subtract_tokens cost_after_cards
    in
    let extra_cost : int =
      List.fold_left (fun sum (_, count) -> sum + count) 0 cost_after_tokens
    in
    let gold_count : int =
      let (_, _, _, tokens) = player in
      let (_, count) = List.find (fun (t, _) -> t = Data.Gold) tokens in
      count
    in
    gold_count >= extra_cost

  let rec can_take (tokens : Data.tokens) (color : Data.color) : bool =
    match tokens with
    | [] -> false
    | (Data.Gold, count) :: tail -> can_take tail color
    | (Data.Normal c, count) :: tail ->
        if c = color then count > 0
        else can_take tail color

  let rec can_take_two (tokens : Data.tokens) (color : Data.color) : bool =
    match tokens with
    | [] -> false
    | (Data.Gold, count) :: tail -> can_take_two tail color
    | (Data.Normal c, count) :: tail ->
        if c = color then count > 1
        else can_take_two tail color

  let card_present (board : Data.board) (card : Data.card) : bool =
    let (tier1, tier2, tier3, tokens, _) = board in
    let hit c = (c = card) in
    List.exists hit tier1 || List.exists hit tier2 || List.exists hit tier3

  let can_return (player : Data.player) (tokens : Data.tokens) : bool =
    let (_, _, _, player_tokens) = player in
    let rec has_tokens player_tokens (color, count) : bool =
      match player_tokens with
      | [] -> count = 0
      | (player_color, player_count) :: tail ->
          if player_color = color then player_count >= count
          else has_tokens tail (color, count)
    in
    List.for_all (has_tokens player_tokens) tokens
end;;

