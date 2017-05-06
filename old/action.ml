open Data;;
open Validation;;

(* Definition of a Player Turn *)
module Action = struct
  type turn = Three of Data.color * Data.color * Data.color |
    Reserve of Data.card |
    Two of Data.color |
    Purchase of Data.card |
    Return of Data.tokens

  let valid_move (state : Data.state) (turn : turn) : bool =
    let (_, _, _, tokens, _) as board, player = state in
    match turn with
    | Three (one, two, three) ->
        let all_different = one <> two && two <> three && one <> three in
        let desired : Data.color list = one :: two :: three :: [] in
        let all_available =
          List.for_all (fun color -> Validation.can_take tokens color) desired in
        all_different && all_available
    | Reserve card ->
        Validation.card_present board card
    | Two color ->
        Validation.can_take_two tokens color
    | Purchase card ->
        Validation.card_present board card && Validation.can_purchase player card
    | Return tokens ->
        Validation.can_return player tokens

  let process (state : Data.state) turn : Data.state =
    state
end;;

