open Read;;

let deck = read_deck "game_data/tier1.txt" in
List.iter (fun card -> Printf.printf "%s\n" (Data.verbose_string_of_card card)) deck
;;
