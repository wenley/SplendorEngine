open Data;;
open Parse;;

let card = card_for_string "9G00000" in
match card with
| Some ({ score = s; color = cr; cost = _ }) ->
    print_string (Printf.sprintf "%d %s" s (string_of_color cr))
| None -> print_string "could not parse"
;;

let noble = noble_for_string "1 00000" in
match noble with
| Some ({ score=s; cost=_ }) ->
    Printf.printf "%d noble\n" s
| None -> print_string "could not parse\n"
;;
