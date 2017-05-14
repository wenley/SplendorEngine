open Data;;
open Player;;
open Board;;
open Validation;;

let buy_test_case (player_tokens:tokens) (card_cost:cost) (expected:bool) : unit =
  let player =
    {
      name="test";
      tokens=player_tokens;
      nobles=[];
      cards=[];
      reserved=[];
    }
  in let board =
    let card = { score=0; color=Blue; cost=card_cost } in
    let empty_deck = { deck=[]; revealed=[] } in
    {
      one={ deck=[]; revealed=card::[] };
      two=empty_deck;
      three=empty_deck;
      nobles=[];
      tokens=TokenMap.empty;
    }
  in ()

