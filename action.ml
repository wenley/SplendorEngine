
type action =
  | Two of Data.color
  | Three of Data.color * Data.color * Data.color
  | Buy of Board.tier_level * int
  | Reserve of Board.tier_level * int
  | ReserveBuy of int
