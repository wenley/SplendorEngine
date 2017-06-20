module type Splendor = sig
  val play : unit -> unit
end

module Splendor (Start:Start.Cookie) (Engine:Engine.Engine) : Splendor = struct
  let play () =
    let initial_game = Start.start_game () in
    let end_game = Engine.play initial_game in
    let winning_player = Winner.winner end_game.Game.players in
    Winner.display_winner winning_player
end

module RealSplendor = Splendor(Start.RealStart)(Engine.RealEngine)
;;

RealSplendor.play ()
