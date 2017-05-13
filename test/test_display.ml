
let cost = Data.ColorMap.empty |> Data.ColorMap.add (Data.Blue) 2 in
let noble = { Data.score=3; Data.cost=cost } in
let nobles = noble :: noble :: [] in
Display.fancy_print_nobles nobles
;;

let cost = Data.ColorMap.empty |> Data.ColorMap.add (Data.Blue) 2 in
let card = { Data.score=3; Data.color=Data.Blue; Data.cost=cost } in
let deck =
  {
    Board.deck=card::card::card::card::[];
    Board.revealed=card::card::[];
  }
in
Display.fancy_print_tier deck
;;
