
module type Start = sig
  val start_game : unit -> Game.game
end

module RealStart : Start
module DummyStart : Start
