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
  let message result =
    Printf.printf "%s : %s%s%s vs %s on board\n"
    result
    (string_of_color c1)
    (string_of_color c2)
    (string_of_color c3)
    (verbose_string_of_tokens board_tokens)
  in
  let action : Action.action = Action.Three(c1, c2, c3) in
  let result = valid action player board in
  match result = expected with
  | true -> message "passed"
  | false -> message "FAILED"
;;

let three_test_cases =
  (Red, Red, Red, [], false)
  :: (Red, Red, Blue, [], false)
  :: (Red, Green, Blue, [], false)
  :: (Red, Green, Blue,
     Normal Red :: Normal Green :: Normal Blue :: [],
     true)
  :: (Red, Green, Blue,
     Gold :: Normal Green :: Normal Blue :: [],
     false)
  :: (Red, Green, Blue,
     Normal Red :: Normal Red :: Normal Green :: Normal Green :: Normal Blue :: Normal Blue :: [],
     true)
  :: []
in let run_test (c1, c2, c3, board_tokens, expected) : unit =
  run_three_test c1 c2 c3 (token_map_of board_tokens) expected
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
  let message result =
    Printf.printf "%s : Player tokens %s vs. Card cost %s\n"
    result
    (verbose_string_of_tokens player_tokens)
    (verbose_string_of_cost card_cost)
  in
  let result = can_purchase player card in
  match result = expected with
  | true -> message "passed"
  | false -> message "FAILED"
;;

let can_purchase_test_cases : (token list * color list * bool) list =
  ([], [], true)
  :: ([], Red :: [], false)
  :: (Gold :: [], Red :: [], true)
  :: (Normal Red :: Gold :: [], Red :: [], true)
  :: []
in let run_test (player_tokens, card_cost, expected) : unit =
  run_can_purchase_test (token_map_of player_tokens) (cost_of card_cost) expected
in List.iter run_test can_purchase_test_cases
;;


