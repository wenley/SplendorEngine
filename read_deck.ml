
let read_lines (file : in_channel) : string list =
  []

let read_deck (filename : string) : card list =
  let file = open_in filename in
  let lines = read_lines file in
  close_in file ;
  let maybe_cards =
    List.map Parse.card_for_string lines
  in
  let is_not_none opt =
    match opt with
    | Some _ -> true
    | None -> false
  in
  let good_cards = 
    List.filter is_not_none maybe_cards
  in
  let unpack opt =
    match opt with 
    | Some value -> value
    | None -> failwith "Cannot unpack None"
  in
  List.map unpack good_cards
