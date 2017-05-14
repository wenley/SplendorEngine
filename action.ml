
type action =
  | Two of Data.color
  | Three of Data.color * Data.color * Data.color
  | Buy of Board.tier_level * int
  | Reserve of Board.tier_level * int
  | ReserveBuy of int

let verbose_string_of_action (action:action) : string =
  match action with
  | Two color -> Printf.sprintf "2 of %s" (Data.verbose_string_of_color color)
  | Three (c1, c2, c3) ->
      Printf.sprintf "3 of (%s, %s, %s)"
        (Data.verbose_string_of_color c1)
        (Data.verbose_string_of_color c2)
        (Data.verbose_string_of_color c3)
  | Buy (level, index) -> Printf.sprintf "Buy Card #%d of Tier %d" index (Board.int_of_tier level)
  | Reserve (level, index) -> Printf.sprintf "Reserve Card #%d of Tier %d" index (Board.int_of_tier level)
  | ReserveBuy index -> Printf.sprintf "Buy Reserved Card #%d" index
