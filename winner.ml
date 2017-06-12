
let find_by (chose:int -> int -> int) (init:int) (score:'a -> int) (things:'a list) : 'a
list =
  let scores = List.map score things in
  let things_with_scores = List.combine things scores in
  let wanted_score = List.fold_left chose init scores in
  let winners =
    List.filter (fun (_, score) -> score = wanted_score) things_with_scores in
  List.map (fun (thing, _) -> thing) winners

let max_by = find_by max 0
let min_by = find_by min 100

let score_winners = max_by Player.score_for_player
let card_winners = min_by (fun p -> List.length p.Player.cards)

let (>>>) players picker_func =
  match players with
  | [] -> []
  | player :: [] -> players
  | players -> picker_func players

let winner players : Player.player =
  let winning_players = players
    >>> score_winners
    >>> card_winners
  in match winning_players with
  | [] -> Player.empty_player "NO WINNER"
  | winner :: [] -> winner
  | winners -> Player.empty_player "MANY WINNERS"

let display_winner player : unit =
  Printf.printf "- - - - - - - - - - - - - - - - - - -\n";
  Printf.printf "Winner is %s with %d points, %d cards\n"
    player.Player.name
    (Player.score_for_player player)
    (List.length player.Player.cards)
  ;
  Printf.printf "- - - - - - - - - - - - - - - - - - -\n"
