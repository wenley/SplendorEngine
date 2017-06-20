module type Engine = sig
  val play : Game.game -> Game.game
end
