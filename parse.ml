open Globals
open Data

let num = "\\([0-9]\\)"
let color = "\\([KBGRW]\\)"
let token = "\\([KBGRWY]\\)"

let card_for_string (input : string) : card option =
  let pattern = num ^ color ^ num ^ num ^ num ^ num ^ num in
  let regex = Str.regexp pattern in
  match Str.string_match regex input 0 with
  | true ->
      let score = int_of_string (Str.matched_group 1 input) in
      let color =
        let color_string = (Str.matched_group 2 input) in
        match color_of_string color_string with
        | Some c -> c
        | None -> failwith ("Color " ^ color_string ^ " was regex matched, but
        not parsed")
      in
      let black_cost = int_of_string (Str.matched_group 3 input) in
      let blue_cost  = int_of_string (Str.matched_group 4 input) in
      let green_cost = int_of_string (Str.matched_group 5 input) in
      let red_cost   = int_of_string (Str.matched_group 6 input) in
      let white_cost = int_of_string (Str.matched_group 7 input) in
      let cost =
        ColorMap.empty |> ColorMap.add Black black_cost
                       |> ColorMap.add Blue blue_cost
                       |> ColorMap.add Green green_cost
                       |> ColorMap.add Red red_cost
                       |> ColorMap.add White white_cost
      in
      let card = { score = score; color = color; cost = cost } in
      Some card
  | false -> None
;;

let noble_for_string (input:string) : noble option =
  let pattern = num ^ " " ^ num ^ num ^ num ^ num ^ num in
  let regex = Str.regexp pattern in
  match Str.string_match regex input 0 with
  | false -> None
  | true ->
      let score = int_of_string (Str.matched_group 1 input) in
      let black_cost = int_of_string (Str.matched_group 2 input) in
      let blue_cost  = int_of_string (Str.matched_group 3 input) in
      let green_cost = int_of_string (Str.matched_group 4 input) in
      let red_cost   = int_of_string (Str.matched_group 5 input) in
      let white_cost = int_of_string (Str.matched_group 6 input) in
      let cost =
        ColorMap.empty |> ColorMap.add Black black_cost
                       |> ColorMap.add Blue blue_cost
                       |> ColorMap.add Green green_cost
                       |> ColorMap.add Red red_cost
                       |> ColorMap.add White white_cost
      in
      let noble = { score=score; cost=cost } in
      Some noble

(* Testing *)

(*let card = card_for_string "9G00000" in
match card with
| Some ({ score = s; color = cr; cost = _ }) ->
    print_string (Printf.sprintf "%d %s" s (string_of_color cr))
| None -> print_string "could not parse"
;;*)
