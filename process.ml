
let process_three c1 c2 c3 player board =
  let new_player =
    let new_tokens = player.Player.tokens |>
    Data.TokenCounter.increment (Data.Normal c1) |>
    Data.TokenCounter.increment (Data.Normal c2) |>
    Data.TokenCounter.increment (Data.Normal c3)
    in { player with Player.tokens=new_tokens }
  in
  let new_board =
    let new_tokens = board.Board.tokens |>
    Data.TokenCounter.decrement (Data.Normal c1) |>
    Data.TokenCounter.decrement (Data.Normal c2) |>
    Data.TokenCounter.decrement (Data.Normal c3)
    in { board with Board.tokens=new_tokens }
  in
  (new_player, new_board)

let process_two color player board =
  let new_player =
    let new_tokens = player.Player.tokens |>
    Data.TokenCounter.increment (Data.Normal color) |>
    Data.TokenCounter.increment (Data.Normal color)
    in { player with Player.tokens=new_tokens }
  in
  let new_board = 
    let new_tokens = board.Board.tokens |>
    Data.TokenCounter.decrement (Data.Normal color) |>
    Data.TokenCounter.decrement (Data.Normal color)
    in { board with Board.tokens=new_tokens }
  in
  (new_player, new_board)

let process_buy card player board =
  let tokens_used : Data.tokens =
    let discount = Player.total_discount player in
    let discounted_cost = Data.apply_discount discount card.Data.cost in
    let tokens_needed = Data.tokens_of_cost discounted_cost in
    let capped_tokens, golds = Data.cover_with_golds player.Player.tokens tokens_needed in
    Data.TokenMap.add Data.Gold golds capped_tokens |>
    Data.TokenMap.filter (fun _ value -> value > 0)
  in
  let new_player =
    let new_tokens =
      let subtract _ player_count needed =
        match player_count, needed with
        | _, None -> player_count
        | None, _ -> raise (Invalid_argument "should be validated")
        | Some player_count, Some needed -> Some (player_count - needed)
      in
      Data.TokenMap.merge subtract player.Player.tokens tokens_used |>
      Data.TokenMap.filter (fun _ value -> value > 0)
    in
    let new_cards = card :: player.Player.cards in
    { player with Player.tokens=new_tokens; Player.cards=new_cards }
  in
  let new_board =
    let new_tokens =
      let add _ board_count returned = Some (board_count + returned) in
      Data.TokenMap.union add board.Board.tokens tokens_used in
    { board with Board.tokens=new_tokens }
  in
  (new_player, new_board)

let process_reserve card player board =
  let has_gold =
    try Data.TokenMap.find Data.Gold board.Board.tokens > 0
    with Not_found -> false
  in
  let new_player =
    let new_reserved = card :: player.Player.reserved in
    let new_tokens =
      if has_gold then Data.TokenCounter.increment Data.Gold player.Player.tokens
      else player.Player.tokens
    in
    { player with Player.reserved=new_reserved; Player.tokens=new_tokens }
  in
  let new_board =
    let new_tokens =
      if has_gold then Data.TokenCounter.decrement Data.Gold board.Board.tokens
      else board.Board.tokens
    in
    { board with Board.tokens=new_tokens }
  in
  (new_player, new_board)

let process_action action player board =
  match action with
  | Action.Three (c1, c2, c3) ->
      process_three c1 c2 c3 player board
  | Action.Two (color) ->
      process_two color player board
  | Action.Buy (level, index) ->
      (match Board.claim_card_at level index board with
      | None, _ -> raise (Invalid_argument "Buy action should be validated")
      | Some card, board -> process_buy card player board
      )
  | Action.Reserve (level, index) ->
      (match Board.claim_card_at level index board with
      | None, _ -> raise (Invalid_argument "Reserve action should be validated")
      | Some card, board -> process_reserve card player board
      )
  | Action.ReserveBuy (index) ->
      (match Player.remove_reserved index player with
      | None, _ -> raise (Invalid_argument "ReserveBuy action should be validated")
      | Some card, player -> process_buy card player board
      )

