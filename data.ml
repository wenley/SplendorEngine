
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

module Action = struct
  type turn = Three of Data.color * Data.color * Data.color |
    Reserve of Data.card |
    Two of Data.color |
    Purchase of Data.card |
    Return of Data.tokens

  let valid_move (state : Data.state) (turn : turn) : bool =
    let (_, _, _, tokens, _) as board, player = state in
    match turn with
    | Three (one, two, three) ->
        let all_different = one <> two && two <> three && one <> three in
        let desired : Data.color list = one :: two :: three :: [] in
        let all_available =
          List.for_all (fun color -> Validation.can_take tokens color) desired in
        all_different && all_available
    | Reserve card ->
        Validation.card_present board card
    | Two color ->
        Validation.can_take_two tokens color
    | Purchase card ->
        Validation.card_present board card && Validation.can_purchase player card
    | Return tokens ->
        Validation.can_return player tokens

  let process (state : Data.state) turn : Data.state =
    state
end;;

module Noble = struct
  let is_eligible (player : Data.player) (noble : Data.noble) : bool = false

  let claim_nobles (state : Data.state) : Data.state =
    let (tier1, tier2, tier3, board_tokens, nobles), player = state in

    let claimed = List.filter (is_eligible player) nobles in
    let (cards, reserved, player_nobles, player_tokens) = player in
    let new_player_nobles = List.append player_nobles claimed in
    let new_player = (cards, reserved, new_player_nobles, player_tokens) in

    let unclaimed = List.filter (fun n -> not (is_eligible player n)) nobles in
    let new_board = (tier1, tier2, tier3, board_tokens, unclaimed) in

    (new_board, new_player)
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

module Engine = struct
  type game = Data.board * Data.player list

  let get_action () = Some (Action.Three(Data.Blue, Data.Red, Data.White))

  let validation (state : Data.state) (turn : Action.turn) : Action.turn option =
    if Action.valid_move state turn then Some turn
    else None

  let take_turn (state : Data.state) (turn : Action.turn) : Data.state option =
    let post_validation = validation state turn in
    let post_process = Option.map (Action.process state) post_validation in
    let post_noble_claim = Option.map Noble.claim_nobles post_process in
    post_noble_claim

  let prompt_players () = []

  let start_game () : game =
    let players = prompt_players () in
    (([], [], [], [], []), players)

  let rec play_round game : game * bool =
    match game with
    | (board, []) -> (game, false)
    | (board, player :: rest) ->
        match get_action () with
        | None -> (game, true)
        | Some action ->
            match take_turn (board, player) action with
            | None -> (game, false)
            | Some (new_board, new_player) ->
                let game_end = Data.total_score new_player >= 15 in
                let other_players, other_game_end = play_round (new_board, rest) in
                (new_board, new_player :: rest), game_end || other_game_end

  let rec game_loop game : game =
    match play_round game with
    | (new_game, true) -> new_game
    | (new_game, false) -> game_loop new_game

  let rec play_game : game =
    let game = start_game () in
    let final_game = game_loop game in
    final_game

end;;

