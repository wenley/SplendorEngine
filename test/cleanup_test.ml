
(**
 * This doesn't work!
 * Rakefile needs to stream Stdout instead of collecting the final string
 *)
let tokens = Data.TokenMap.empty |> Data.TokenMap.add (Data.Normal Data.Black) 12 in
let player = { (Player.empty_player "You") with Player.tokens = tokens } in
Cleanup.discard_tokens player
;;
