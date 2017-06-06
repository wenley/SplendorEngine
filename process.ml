
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

let process_action action player board =
  match action with
  | Action.Three (c1, c2, c3) ->
      process_three c1 c2 c3 player board
  | Action.Two (color) ->
      process_two color player board
  | _ -> (player, board)

