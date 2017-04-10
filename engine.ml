open Data;;

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

