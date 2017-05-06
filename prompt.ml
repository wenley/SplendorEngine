open Data;;
open Action;;
open Printf;;

(* Module for all processing of strings of raw user-input *)
module Prompt = struct
  let rec prompt_players player_number () : Data.player list =
    let prompt = sprintf "Enter name for player #%d: " player_number in
    print_string prompt ;
    match String.trim (read_line ()) with
    | "" -> 
        let message = sprintf "No more players\n" in
        print_string message ;
        []
    | s ->
        let message = sprintf "Player %d is %s\n" player_number s in
        print_string message ;
        (s, [], [], [], []) :: prompt_players (player_number + 1) ()

  let parse_three (input : string list) : Action.turn option =
    match input with
    | s1 :: s2 :: s3 :: [] ->
        (match (color_for_string s1, color_for_string s2, color_for_string s3) with
        | (Some c1, Some c2, Some c3) ->
            Some (Action.Three (c1, c2, c3))
        | _ -> None)
    | _ -> None

  let parse_three (input : string) : Action.turn option =
    let pattern = Str.regexp "3 \\([BKGRW]\\) \\([BKGRW]\\) \\([BKGRW]\\)" in
    match Str.string_match pattern input 0 with
    | true ->
        let c1 = color_for_string (Str.matched_group 1 input) in
        let c2 = color_for_string (Str.matched_group 2 input) in
        let c3 = color_for_string (Str.matched_group 3 input) in
        (match c1, c2, c3 with
        | (Some c1, Some c2, Some c3) -> Some (Action.Three (c1, c2, c3))
        | _ -> None)
    | false -> None

  let parse_two (input : string) : Action.turn option =
    let pattern = Str.regexp "2 \\([BKGRW]\\)" in
    match Str.string_match pattern input 0 with
    | true ->
        let color = color_for_string (Str.matched_group 1 input) in
        (match color with
        | Some color -> Some (Action.Two (color))
        | _ -> None)
    | false -> None

  let parse_card (input : string) : Action.turn option =
    let pattern = Str.regexp "\\([BRX]\\) \\([0-9]\\)\\([BKGRW]\\) \\([0-9]\\)\\([0-9]\\)\\([0-9]\\)\\([0-9]\\)\\([0-9]\\)" in
    match Str.string_match pattern input 0 with
    | true ->
        let action_type =                  (Str.matched_group 1 input) in
        let score       = int_of_string    (Str.matched_group 2 input) in
        let card_color  = color_for_string (Str.matched_group 3 input) in
        let blacks      = int_of_string    (Str.matched_group 4 input) in
        let blues       = int_of_string    (Str.matched_group 5 input) in
        let greens      = int_of_string    (Str.matched_group 6 input) in
        let reds        = int_of_string    (Str.matched_group 7 input) in
        let whites      = int_of_string    (Str.matched_group 8 input) in
        let card_cost = (Data.Black, blacks) :: (Data.Blue, blues) :: (Data.Green, greens) :: (Data.Red, reds) :: (Data.White, whites) :: [] in
        (match card_color with
        | Some card_color ->
          let card = (score, card_color, card_cost) in
          (match action_type with
          | "B" -> Some (Action.Purchase (card))
          | "R" -> Some (Action.Reserve (card))
          | "X" -> Some (Action.Purchase (card))
          | _ -> None
          )
        | None -> None
        )
    | false -> None

  let parsers = 
    parse_three :: parse_two :: parse_card :: []

  let parse_turn (input : string) : Action.turn option =
    let parse_attempts = List.map (fun p -> p input) parsers in
    let parse_success attempt = 
      match attempt with
      | Some _ -> true
      | None -> false
    in
    let parse_success = maybe_find parse_success parse_attempts in
    match parse_success with
    | Some (maybe_action) -> maybe_action
    | None -> 
        Some (Action.Purchase (0, Data.Blue, []))

  let print_turn_help () =
    print_string "Proper turn input format:\n" ;
    print_string "  3 <color> <color> <color> = Take three tokens\n" ;
    print_string "  2 <color> = Take two tokens\n" ;
    print_string "  B <score><color> <black_cost><blue_cost><green_cost><red_cost><white_cost>\n" ;
    print_string "    = Buy described card\n" ;
    print_string "  R <score><color> <black_cost><blue_cost><green_cost><red_cost><white_cost>\n" ;
    print_string "    = Reserve described card\n" ;
    print_string "  X <score><color> <black_cost><blue_cost><green_cost><red_cost><white_cost>\n" ;
    print_string "    = Buy described card from reserve\n"

  let rec prompt_turn () : Action.turn =
    match String.trim (read_line ()) with
    | "" -> (print_turn_help () ; prompt_turn ())
    | something ->
        match parse_turn something with
        | None ->
            print_string "Invalid turn.";
            print_turn_help ();
            prompt_turn ()
        | Some action -> action
end;;

