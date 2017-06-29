module type Engine = sig
  val play : Game.game -> Game.game
end

module Engine (G:Game.Game) : Engine
module RealEngine : Engine
