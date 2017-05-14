
let enough_tokens (required:Data.tokens) (tokens:Data.tokens) : bool =
  true

let board_has (tokens:Data.tokens) (board:Board.board) : bool =
  true

let valid action player board =
  match action with
  | Three (c1, c2, c3) ->
      let all_different = (c1 <> c2) && (c2 <> c3) && (c1 <> c3) in
      let wanted = Data.TokenMap.empty
        |> Data.TokenMap.add (Normal c1) 1
        |> Data.TokenMap.add (Normal c2) 1
        |> Data.TokenMap.add (Normal c3) 1
      in
      let available = board_has wanted board in
      all_different && available
  | Two (color) ->
      let wanted = Data.TokenMap.empty |> Data.TokenMap.add (Normal color) 2 in
      let available = board_has wanted board in
      let needed = Data.TokenMap.empty |> Data.TokenMap.add (Normal color) 4 in
      let allowed = board_has needed board in
      allowed && available
