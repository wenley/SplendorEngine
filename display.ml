
(*
Noble:
+-----+
|    3|
|   K2|
|   W2|
|   R2|
+-----+

Card:
+-----+
|K   5|
|   K1|
|   G2|
|   W2|
|   R2|
+-----+

Deck:
+-----+
|Cards|
|10   |
|     |
|     |
|     |
+-----+

 *)
let border = "+-----+"
let empty_line = "|     |"

let rec pad (length:int) (padding:string) (strings:string list) =
  let list_length = List.length strings in
  if list_length >= length then strings
  else
    pad length padding (padding :: strings)

let rec repeat_print (n:int) (s:string) : unit =
  if n <= 0 then ()
  else (print_string s ; repeat_print (n-1) s)

let print_top_border (n:int) : unit =
  repeat_print n border

(**
 * NOTE: Silent weird behavior if:
 * strings are different lengths
 * sublists are different lengths
 *)
let rec parallel_prints (strings:string list list) : unit =
  if List.length (List.hd strings) <= 0 then ()
  else
  let rec print_and_next (strings:string list) : string list =
    match strings with
    | [] -> []
    | hd :: tl ->
        print_string hd; tl
  in
  let one_less_line = List.map print_and_next strings in
  print_newline () ;
  parallel_prints one_less_line

(* Printing for Nobles *)
let cost_lines (cost:Data.cost) : string list =
  let cost_line (color:Data.color) (n:int) (lines:string list) : string list =
    let line = Printf.sprintf "|   %s%d|" (Data.string_of_color color) n in
    line :: lines
  in
  Data.ColorMap.fold cost_line cost []

let noble_strings (noble:Data.noble) : string list =
  let noble_score_line = Printf.sprintf "|%5d|" noble.Data.score in
  let noble_cost_lines = 
    let raw_lines = cost_lines noble.Data.cost in
    pad 3 empty_line raw_lines
  in
  border :: noble_score_line :: noble_cost_lines @ border :: []

let fancy_print_nobles (nobles:Data.noble list) : unit =
  let header_line = "Nobles:   " in
  let blank_line = "          " in
  let left_padding = pad 6 blank_line (header_line :: (pad 3 blank_line [])) in
  let noble_lines = List.map noble_strings nobles in
  let all_lines = left_padding :: noble_lines in
  parallel_prints all_lines

(* Printing for a Tier *)
let deck_strings (stack:Data.card list) (header:string) : string list =
  let card_count = List.length stack in
  let count_line = Printf.sprintf "|%5d|" card_count in
  let header_line = Printf.sprintf "|%5s|" header in
  border :: header_line :: "|Cards|" :: count_line ::
    empty_line :: empty_line :: border :: []

let card_strings (card:Data.card) : string list =
  let first_line =
    Printf.sprintf "|%s%4d|" (Data.string_of_color card.Data.color) card.Data.score
  in
  let raw_costs = cost_lines card.Data.cost in
  let costs = pad 4 empty_line raw_costs in
  border :: first_line :: costs @ border:: []

let tier_spacer_strings : string list =
  pad 7 "   " []

let fancy_print_tier (deck:Data.card Board.deck) (header:string) : unit =
  let strings =
    deck_strings deck.Board.deck header
    :: tier_spacer_strings
    :: List.map card_strings deck.Board.revealed
  in
  parallel_prints strings

(* Printing for Tokens *)
let fancy_print_tokens (tokens:Data.tokens) : unit =
  let token_lines : string list =
    let add_token (token:Data.token) (count:int) (stack:string list) : string list =
      Printf.sprintf "%5s: %2d" (verbose_string_of_token token) count :: stack
    in
    Data.TokenMap.fold add_token tokens [] in
  let header_lines : string list =
    let blank_line = "          " in
    let header_line = Printf.sprintf "%10s" "Tokens:" in
    header_line :: (pad 5 blank_line [])
  in
  let all_lines = header_lines :: token_lines :: [] in
  parallel_prints all_lines

(* Printing for Board *)
let fancy_print_board (board:Board.board) : unit =
  fancy_print_nobles board.Board.nobles;
  print_newline ();
  fancy_print_tier board.Board.three "Tier3";
  fancy_print_tier board.Board.two "Tier2";
  fancy_print_tier board.Board.one "Tier1"
  fancy_print_tokens board.Board.tokens

