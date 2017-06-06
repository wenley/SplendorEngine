open Data;;
open Player;;
open Board;;
open Process;;
open Shared;;

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
