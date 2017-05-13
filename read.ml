
let rec read_lines (file : in_channel) : string list =
  let maybe_line =
    try Some (input_line file)
    with End_of_file -> None
  in
  match maybe_line with
  | Some line -> line :: (read_lines file)
  | None -> []

let lines_in_file (filename:string) : string list =
  let file = open_in filename in
  let lines = read_lines file in
  close_in file ;
  lines

let read_data (filename : string) (parse:string -> 'a option) : 'a list =
  let lines = lines_in_file filename in
  let maybe_items = List.map parse lines in
  let good_items = 
    let is_not_none opt =
      match opt with
      | Some _ -> true
      | None -> false
    in
    List.filter is_not_none maybe_items
  in
  let unpack opt =
    match opt with 
    | Some value -> value
    | None -> failwith "Cannot unpack None"
  in
  List.map unpack good_items

let read_nobles (filename:string) : Data.noble list =
  read_data filename Parse.noble_for_string

let read_deck (filename : string) : Data.card list =
  read_data filename Parse.card_for_string
