
(* Testing Code *)
let player = {
  Player.name = "Me";
  Player.tokens = Data.TokenMap.empty |> Data.TokenMap.add (Data.Normal Data.Black) 2;
  Player.nobles = [];
  Player.cards = [];
  Player.reserved = [];
} in
Printf.printf "%s\n" (Player.string_of_player player)
