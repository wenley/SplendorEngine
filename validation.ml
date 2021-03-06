
let enough_tokens (required:Data.tokens) (tokens:Data.tokens) : bool =
  let tokens_has (token:Data.token) (count:int) : bool =
    let tokens_value = try Data.TokenMap.find token tokens with Not_found -> 0 in
    count <= tokens_value
  in
  Data.TokenMap.for_all tokens_has required

let tokens_cover (required:Data.cost) (tokens:Data.tokens) : bool =
  let token_cost = Data.tokens_of_cost required in
  let _, golds_needed = Data.cover_with_golds tokens token_cost in
  let golds_have =
    try Data.TokenMap.find Data.Gold tokens
    with Not_found -> 0
  in
  golds_needed <= golds_have

let board_has (tokens:Data.tokens) (board:Board.board) : bool =
  enough_tokens tokens board.Board.tokens

let can_purchase (player:Player.player) (card:Data.card) : bool =
  let discount = Player.total_discount player in
  let discounted_cost = Data.apply_discount discount card.Data.cost in
  tokens_cover discounted_cost player.Player.tokens

let valid action player board =
  match action with
  | Action.Three (c1, c2, c3) ->
      let all_different = (c1 <> c2) && (c2 <> c3) && (c1 <> c3) in
      let wanted = Data.TokenMap.empty
        |> Data.TokenMap.add (Data.Normal c1) 1
        |> Data.TokenMap.add (Data.Normal c2) 1
        |> Data.TokenMap.add (Data.Normal c3) 1
      in
      let available = board_has wanted board in
      all_different && available
  | Action.Two (color) ->
      let needed = Data.TokenMap.empty |> Data.TokenMap.add (Data.Normal color) 4 in
      board_has needed board
  | Action.Buy (tier_level, index) ->
      (match Board.card_at tier_level index board with
      | None -> false
      | Some card_wanted -> can_purchase player card_wanted
      )
  | Action.Reserve (tier_level, index) ->
      let reserved_count = List.length player.Player.reserved in
      reserved_count < 3
  | Action.ReserveBuy index ->
      (match Player.reserve_card_at index player with
      | None -> false
      | Some card_wanted -> can_purchase player card_wanted
      )

