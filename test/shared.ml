open Data;;
open Player;;
open Board;;

let empty_card = { score=0; color=Blue; cost=ColorMap.empty }
;;

let empty_deck = { deck=[]; revealed=[] }
;;

let empty_player =
  {
    name="";
    tokens=TokenMap.empty;
    nobles=[];
    cards=[];
    reserved=[];
  }
let empty_board =
  {
    one=empty_deck;
    two=empty_deck;
    three=empty_deck;
    nobles=[];
    tokens=TokenMap.empty;
  }

let rec token_map_of (tokens:token list) : tokens =
  match tokens with
  | [] -> TokenMap.empty
  | hd :: tl -> token_map_of tl |> TokenCounter.increment hd
;;

let rec cost_of (colors:color list) : cost =
  match colors with
  | [] -> ColorMap.empty
  | hd :: tl -> cost_of tl |> ColorCounter.increment hd
;;

