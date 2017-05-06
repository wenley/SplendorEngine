open Data;;

(* Module for printing types in Data *)
module Display = struct
  let color_string (color : Data.color) : string =
    match color with
    | Data.Blue -> "Blue"
    | Data.Black -> "Black"
    | Data.Green -> "Green"
    | Data.Red -> "Red"
    | Data.White -> "White"

  let print_color (color : Data.color) : unit =
    print_string (color_string color)

  let rec print_cost (cost : Data.cost) : unit =
    match cost with
    | [] -> ()
    | (color, count) :: rest ->
        print_color color ;
        print_cost rest

  let print_card (card : Data.card) : unit =
    let score, color, cost = card in
    print_int score ;
    print_color color ;
    print_cost cost

  let print_cards (cards : Data.card list) : unit =
    let print card =
      print_card card ;
      print_newline ()
    in
    List.iter print cards

  let print_noble (noble : Data.noble) : unit =
    let score, cost = noble in
    print_int score ;
    print_cost cost

  let print_nobles nobles =
    let print noble =
      print_noble noble ;
      print_newline ()
    in
    List.iter print nobles

  let rec print_tokens (tokens : Data.tokens) =
    match tokens with
    | [] -> ()
    | (token, count) :: rest ->
        let color_s = 
          match token with
          | Data.Normal color -> color_string color
          | Data.Gold -> "Gold"
        in
        print_string color_s ;
        print_int count ;
        print_tokens rest

  let print_player (player : Data.player) : unit =
    let name, cards, reserved, nobles, tokens = player in
    print_string name ;
    print_cards cards ;
    print_cards reserved ;
    print_nobles nobles ;
    print_tokens tokens

  let print_board (board : Data.board) : unit =
    ()
end;;

let tokens = (Data.Gold, 1) :: (Data.Normal Data.Blue, 2) :: [] in
Display.print_tokens tokens;;
