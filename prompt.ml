
(* Patterns *)
let color = "\\([KBGRW]\\)"
let num = "\\([0-9]\\)"
let tier = "\\([1-3]\\)"
let index = "\\([1-4]\\)"
let reserve_index = tier

let three_pattern = "3" ^ color ^ color ^ color
let two_pattern = "2" ^ color
let buy_pattern = "B" ^ tier ^ index
let reserve_pattern = "R" ^ tier ^ index
let reserve_buy_pattern = "X" ^ reserve_index

let patterns = three_pattern
  :: two_pattern
  :: buy_pattern
  :: reserve_pattern
  :: reserve_buy_pattern
  :: []

let regexes = List.map Str.regexp patterns

let action_pattern =
  let body = Globals.string_of_list (fun s -> s) "|" patterns in
  "(" ^ body ^ ")"

(* Parsers *)
let color_at (index:int) (input:string) : Data.color =
  match Data.color_of_string (Str.matched_group index input) with
  | Some c -> c
  | None -> failwith (Printf.sprintf "Input %s was matched, but didn't parse" input)

let tier_at (index:int) (input:string) : Board.tier_level =
  match Str.matched_group index input with
  | "1" -> Board.One
  | "2" -> Board.Two
  | "3" -> Board.Three
  | _ -> failwith (Printf.sprintf "Input %s was matched, but didn't parse" input)

let parse_three (input:string) =
  let color1 = color_at 1 input in
  let color2 = color_at 2 input in
  let color3 = color_at 3 input in
  Action.Three (color1, color2, color3)

let parse_two (input:string) =
  let color = color_at 1 input in
  Action.Two (color)

let parse_buy (input:string) =
  let tier = tier_at 1 input in
  let index = int_of_string (Str.matched_group 2 input) in
  Action.Buy (tier, index)

let parse_reserve (input:string) =
  let tier = tier_at 1 input in
  let index = int_of_string (Str.matched_group 2 input) in
  Action.Reserve (tier, index)

let parse_reserve_buy (input:string) =
  let index = int_of_string (Str.matched_group 1 input) in
  Action.ReserveBuy (index)

let parsers = parse_three
  :: parse_two
  :: parse_buy
  :: parse_reserve
  :: parse_reserve_buy
  :: []

(* Aggregate Parser *)

let patterns_and_parsers = List.combine regexes parsers

let rec find_action (input:string) pap : Action.action option =
  match pap with
  | [] -> None
  | (regex, parse) :: tl ->
      match Str.string_match regex input 0 with
      | false -> find_action input tl
      | true -> Some (parse input)

let parse_action (input:string) =
  find_action input patterns_and_parsers

let prompt_read_action () : unit =
  Printf.printf "Please enter an action. Action formats:\n";
  Printf.printf "  Take 3 Tokens: 3<color><color><color> (e.g. 3RGB)\n";
  Printf.printf "  Take 2 Tokens: 2<color> (e.g. 2K)\n";
  Printf.printf "  Buy a Card: B<tier><index> (e.g. B11)\n";
  Printf.printf "  Reserve a Card: R<tier><index> (e.g. R11)\n";
  Printf.printf "  Buy a Card from Reserve: X<index> (e.g. X2)\n"

let rec read_action () : Action.action =
  prompt_read_action ();
  match parse_action (read_line ()) with
  | None -> read_action ()
  | Some action -> action
