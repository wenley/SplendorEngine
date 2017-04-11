open Data;;

module Noble = struct
  let is_eligible (player : Data.player) (noble : Data.noble) : bool = false

  let claim_nobles (state : Data.state) : Data.state =
    let (tier1, tier2, tier3, board_tokens, nobles), player = state in

    let claimed = List.filter (is_eligible player) nobles in
    let (name, cards, reserved, player_nobles, player_tokens) = player in
    let new_player_nobles = List.append player_nobles claimed in
    let new_player = (name, cards, reserved, new_player_nobles, player_tokens) in

    let unclaimed = List.filter (fun n -> not (is_eligible player n)) nobles in
    let new_board = (tier1, tier2, tier3, board_tokens, unclaimed) in

    (new_board, new_player)
end;;

