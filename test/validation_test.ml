open Data;;
open Player;;
open Board;;
open Validation;;

let empty_deck = { deck=[]; revealed=[] }
;;

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


let run_three_test (c1:color) (c2:color) (c3:color) (board_tokens:tokens) (expected:bool) : unit =
  let player = {
    name="";
    tokens=TokenMap.empty;
    nobles=[];
    cards=[];
    reserved=[];
  } in
  let board = {
    one=empty_deck;
    two=empty_deck;
    three=empty_deck;
    nobles=[];
    tokens=board_tokens;
  } in
  let message =
    Printf.printf "%s%s%s vs %s on board %s\n"
    (string_of_color c1)
    (string_of_color c2)
    (string_of_color c3)
    (verbose_string_of_tokens board_tokens)
  in
  let action : Action.action = Action.Three(c1, c2, c3) in
  let result = valid action player board in
  match result = expected with
  | true -> message "passed"
  | false -> message "failed"
;;

let three_test_cases =
  (Red, Red, Red, TokenMap.empty, false)
  :: (Red, Red, Blue, TokenMap.empty, false)
  :: (Red, Green, Blue, TokenMap.empty, false)
  :: (Red, Green, Blue, token_map_of (Normal Red :: Normal Green :: Normal Blue :: []), true)
  :: (Red, Green, Blue, token_map_of (Gold :: Normal Green :: Normal Blue :: []), false)
  :: []
in let run_test (c1, c2, c3, board_tokens, expected) : unit =
  run_three_test c1 c2 c3 board_tokens expected
in List.iter run_test three_test_cases
;;

let run_can_purchase_test (player_tokens:tokens) (card_cost:cost) (expected:bool) : unit =
  let player =
    {
      name="test";
      tokens=player_tokens;
      nobles=[];
      cards=[];
      reserved=[];
    } in
  let card = { score=0; color=Blue; cost=card_cost } in
  let message =
    Printf.printf "Player tokens %s vs. Card cost %s %s\n"
    (verbose_string_of_tokens player_tokens)
    (verbose_string_of_cost card_cost)
  in
  let result = can_purchase player card in
  match result = expected with
  | true -> message "passed"
  | false -> message "failed"
;;

let can_purchase_test_cases =
  (token_map_of [], cost_of [], true)
  :: (token_map_of [], cost_of (Red :: []), false)
  :: []
in let run_test (player_tokens, card_cost, expected) : unit =
  run_can_purchase_test player_tokens card_cost expected
in List.iter run_test can_purchase_test_cases
;;


