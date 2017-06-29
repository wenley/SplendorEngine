open Globals
open Data

let num = "\\([0-9]\\)"
let color = "\\([KBGRW]\\)"
let token = "\\([KBGRWY]\\)"

let maybe_add key value map =
  if value > 0 then ColorMap.add key value map
  else map

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
        ColorMap.empty |> maybe_add Black black_cost
                       |> maybe_add Blue blue_cost
                       |> maybe_add Green green_cost
                       |> maybe_add Red red_cost
                       |> maybe_add White white_cost
      in
      let card = { score; color; cost } in
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
        ColorMap.empty |> maybe_add Black black_cost
                       |> maybe_add Blue blue_cost
                       |> maybe_add Green green_cost
                       |> maybe_add Red red_cost
                       |> maybe_add White white_cost
      in
      let noble = { score; cost } in
      Some noble
