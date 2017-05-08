

let rec read_lines (file : in_channel) : string list =
  let maybe_line =
    try Some (input_line file)
    with End_of_file -> None
  in
  match maybe_line with
  | Some line -> line :: (read_lines file)
  | None -> []

let read_deck (filename : string) : Data.card list =
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
;;

(* Simple testing things *)

(*let deck = read_deck "tier1.txt" in
List.iter (fun card -> Printf.printf "%s\n" (Data.verbose_string_of_card card)) deck
;;*)
