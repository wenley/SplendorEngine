
let cost =
  Data.ColorMap.empty |> Data.ColorMap.add (Data.Blue) 2 
                      |> Data.ColorMap.add (Data.White) 3

in
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
Display.fancy_print_tier deck "Head"
;;

let deck = { Board.deck=[]; Board.revealed=[] } in
let nobles = [] in
let tokens =
  Data.TokenMap.empty |> Data.TokenMap.add Data.Gold 2 
                      |> Data.TokenMap.add (Data.Normal Data.Black) 0
                      |> Data.TokenMap.add (Data.Normal Data.Blue) 0
                      |> Data.TokenMap.add (Data.Normal Data.Green) 0
                      |> Data.TokenMap.add (Data.Normal Data.Red) 0
                      |> Data.TokenMap.add (Data.Normal Data.White) 0
in
let board = {
  Board.one=deck;
  Board.two=deck;
  Board.three=deck;
  Board.nobles=nobles;
  Board.tokens=tokens;
} in
Display.fancy_print_board board
;;
