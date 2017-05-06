open Data;;
open Action;;

module Parse = struct
  let parse_card string : Data.card =
    (0, Data.Blue, [])

  let parse_turn string : Action.turn option =
    Some (Action.Purchase (0, Data.Blue, []))

  let parse_action string : Action.turn option =
    Some (Action.Three(Data.Blue, Data.Red, Data.White))
end;;

