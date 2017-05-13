
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
  let all_lines = List.map noble_strings nobles in
  parallel_prints all_lines

let deck_strings (stack:Data.card list) : string list =
  let card_count = List.length stack in
  let count_line = Printf.sprintf "|%5d|" card_count in
  border :: "|Cards|" :: count_line ::
    empty_line :: empty_line :: empty_line :: border :: []

let card_strings (card:Data.card) : string list =
  let first_line =
    Printf.sprintf "|%s%4d|" (Data.string_of_color card.Data.color) card.Data.score
  in
  let raw_costs = cost_lines card.Data.cost in
  let costs = pad 4 empty_line raw_costs in
  border :: first_line :: costs @ border:: []

let tier_spacer_strings : string list =
  pad 7 "   " []

let fancy_print_tier (deck:Data.card Board.deck) : unit =
  let strings =
    deck_strings deck.Board.deck
    :: tier_spacer_strings
    :: List.map card_strings deck.Board.revealed
  in
  parallel_prints strings

let fancy_print_board (board:Board.board) : unit =
  Printf.printf "Nobles:\n";
  fancy_print_nobles board.Board.nobles;
  print_newline ();
  fancy_print_tier board.Board.three;
  fancy_print_tier board.Board.two;
  fancy_print_tier board.Board.one
  (* Print tokens *)

