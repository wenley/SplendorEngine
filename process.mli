
val process_buy : Data.card -> Player.player -> Board.board ->
  (Player.player * Board.board)

val process_action : Action.action -> Player.player -> Board.board ->
  (Player.player * Board.board)
