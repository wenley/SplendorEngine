open Data;;
open Player;;
open Board;;
open Process;;
open Shared;;

(* Test Take Three Action *)

let run_process_three_test c1 c2 c3
  (before_player:tokens) (after_player:tokens)
  (before_board:tokens) (after_board:tokens) : unit =
  let player = { empty_player with tokens=before_player } in
  let board = { empty_board with tokens=before_board } in
  let action = Action.Three(c1, c2, c3) in
  let (new_player, new_board) = process_action action player board in
  let new_player_tokens = new_player.tokens in
  let new_board_tokens = new_board.tokens in
  let message result =
    Printf.printf "%s : Player (%s vs %s) Board (%s vs %s)\n"
    result
    (verbose_string_of_tokens new_player_tokens)
    (verbose_string_of_tokens after_player)
    (verbose_string_of_tokens new_board_tokens)
    (verbose_string_of_tokens after_board)
  in
  match (TokenMap.equal (=) new_player_tokens after_player &&
  TokenMap.equal (=) new_board_tokens after_board)
  with
  | true -> message "passed"
  | false -> message "FAILED"
;;

let before_player = (token_map_of []) in
let after_player = (token_map_of (Normal Red :: Normal Green :: Normal Blue ::
  [])) in
let before_board = after_player in
let after_board = before_player in
run_process_three_test Red Green Blue before_player after_player before_board
after_board
;;

let before_player = (token_map_of []) in
let after_player = (token_map_of (Normal Red :: Normal Green :: Normal Blue ::
  [])) in
let before_board = (token_map_of (Normal Red :: Normal Green :: Normal Blue ::
  Normal Red :: Normal Green :: Normal Blue ::
  [])) in
let after_board = after_player in
run_process_three_test Red Green Blue before_player after_player before_board
after_board
;;

(* Test Take Two Action *)

let run_process_two_test color before_player after_player before_board
  after_board : unit =
  let player = { empty_player with tokens=before_player } in
  let board = { empty_board with tokens=before_board } in
  let action = Action.Two(color) in
  let (new_player, new_board) = process_action action player board in
  let new_player_tokens = new_player.tokens in
  let new_board_tokens = new_board.tokens in
  let message result =
    Printf.printf "%s : Player (%s vs %s) Board (%s vs %s)\n"
    result
    (verbose_string_of_tokens new_player_tokens)
    (verbose_string_of_tokens after_player)
    (verbose_string_of_tokens new_board_tokens)
    (verbose_string_of_tokens after_board)
  in
  match (TokenMap.equal (=) new_player_tokens after_player &&
  TokenMap.equal (=) new_board_tokens after_board)
  with
  | true -> message "passed"
  | false -> message "FAILED"
;;

let before_player = (token_map_of []) in
let after_player = (token_map_of (Normal Red :: Normal Red :: [])) in
let before_board = after_player in
let after_board = before_player in
run_process_two_test Red before_player after_player before_board after_board
;;

let before_player = (token_map_of []) in
let after_player = (token_map_of (Normal Red :: Normal Red :: [])) in
let before_board = (token_map_of (Normal Red :: Normal Red :: Normal Red :: [])) in
let after_board = (token_map_of (Normal Red :: [])) in
run_process_two_test Red before_player after_player before_board after_board
;;

let before_player = (token_map_of []) in
let after_player = (token_map_of (Normal Red :: Normal Red :: [])) in
let before_board = (token_map_of (Normal Red :: Normal Red :: Normal Red ::
  Normal Red :: [])) in
let after_board = after_player in
run_process_two_test Red before_player after_player before_board after_board
;;

(* Test Buy Action *)

let run_process_buy_test card before_player after_player before_board
after_board : unit =
  let new_player, new_board = process_buy card before_player before_board in
  let message result =
    Printf.printf "%s:\n  Player %s vs %s\n  Board %s vs %s\n\n"
    result
    (string_of_player new_player)
    (string_of_player after_player)
    (string_of_board new_board)
    (string_of_board after_board)
  in
  match (after_player = new_player && after_board = new_board) with
  | true -> Printf.printf "passed : buy test\n"
  | false -> message "FAILED"
;;

let card =
  let cost = cost_of (Red :: Red :: []) in
  { empty_card with cost=cost }
in
let before_player =
  let cards = { empty_card with color=Red } :: [] in
  let tokens = token_map_of (Normal Red :: []) in
  { empty_player with cards=cards; tokens=tokens }
in
let after_player =
  let cards = card :: before_player.cards in
  let tokens = token_map_of [] in
  { empty_player with cards=cards; tokens=tokens }
in
let before_board = empty_board in
let after_board =
  let tokens = token_map_of (Normal Red :: []) in
  { empty_board with tokens=tokens }
in
run_process_buy_test card before_player after_player before_board after_board
;;

(* Test Reserve Action *)
let run_process_reserve_test before_player after_player before_board after_board
: unit =
  let action = Action.Reserve(One, 0) in
  let new_player, new_board =
    process_action action before_player before_board
  in
  let message result =
    Printf.printf "%s:\n  Player %s vs %s\n  Board %s vs %s\n\n"
    result
    (string_of_player new_player)
    (string_of_player after_player)
    (string_of_board new_board)
    (string_of_board after_board)
  in
  match (after_player = new_player && after_board = new_board) with
  | true -> Printf.printf "passed : reserve test\n"
  | false -> message "FAILED"
;;

(* Also gets a gold token *)
let before_player = empty_player in
let after_player =
  let reserved = empty_card::[] in
  let tokens = token_map_of (Gold :: []) in
  { empty_player with reserved=reserved; tokens=tokens }
in
let before_board =
  let tier = { deck=[]; revealed=empty_card::[] } in
  let tokens = token_map_of (Gold :: []) in
  { empty_board with one=tier; tokens=tokens }
in
let after_board = empty_board in
run_process_reserve_test before_player after_player before_board after_board
;;

(* No Gold tokens to take *)
let before_player = empty_player in
let after_player =
  let reserved = empty_card::[] in
  { empty_player with reserved=reserved }
in
let before_board =
  let tier = { deck=[]; revealed=empty_card::[] } in
  { empty_board with one=tier }
in
let after_board = empty_board in
run_process_reserve_test before_player after_player before_board after_board
;;

(* Test ReserveBuy Action *)

let run_process_reserve_buy_test card before_player after_player before_board
after_board : unit =
  ()
