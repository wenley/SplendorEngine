let empty_card = { Data.score=0; Data.color=Data.Blue; Data.cost=Data.ColorMap.empty }
;;

let empty_deck = { Board.deck=[]; Board.revealed=[] }
;;

let empty_player = Player.empty_player ""
let empty_board =
  {
    Board.one=empty_deck;
    Board.two=empty_deck;
    Board.three=empty_deck;
    Board.nobles=[];
    Board.tokens=Data.TokenMap.empty;
  }

let rec token_map_of (tokens:Data.token list) : Data.tokens =
  match tokens with
  | [] -> Data.TokenMap.empty
  | hd :: tl -> token_map_of tl |> Data.TokenCounter.increment hd
;;

let rec cost_of (colors:Data.color list) : Data.cost =
  match colors with
  | [] -> Data.ColorMap.empty
  | hd :: tl -> cost_of tl |> Data.ColorCounter.increment hd
;;

