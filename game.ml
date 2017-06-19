
type game = {
  players: Player.player list;
    board: Board.board;
}

let game_over (game:game) : bool =
  List.exists (fun p -> Player.score_for_player p >= 15) game.players

module type Game = sig
  val game_over : game -> bool
end

module DefaultGame : Game = struct
  let game_over (game:game) : bool =
    List.exists (fun p -> Player.score_for_player p >= 15) game.players
end

module QuickGame : Game = struct
  let game_over (game:game) : bool =
    List.exists (fun p -> Player.score_for_player p >= 5) game.players
end

module DebugGame : Game = struct
  let game_over (game:game) : bool =
    List.exists (fun p -> List.length p.Player.cards > 0) game.players
end

