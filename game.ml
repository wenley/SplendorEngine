
type game = {
  players: Player.player list;
  board: Board.board;
}

let game_over (game:game) : bool =
  let { players } = game in
  List.exists (fun p -> Player.score_for_player p >= 15) players
