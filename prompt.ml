open Data;;
open Action;;

module Prompt = struct
  let rec prompt_players player_number () : Data.player list =
    print_string "Enter name for player" ;
    print_int player_number ;
    print_newline () ;
    match String.trim (read_line ()) with
    | "" -> []
    | s -> (s, [], [], [], []) :: prompt_players (player_number + 1) ()

  let parse_turn string : Action.turn option =
    Some (Action.Purchase (0, Data.Blue, []))

  let print_turn_help () =
    print_string "Proper turn input format:"

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

