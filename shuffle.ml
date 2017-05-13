
let shuffle (items : 'a list) : 'a list =
  let rec shuffle_list (items:'a list) (len:int) =
    match items, len with
    | ([], 0) -> []
    | ([], _) -> failwith "Empty list with non-zero length"
    | (_, 0) -> failwith "Non-empty list with zero length"
    | (items, len) ->
      let chosen_index = Random.int len in
      let chosen_item = List.nth items chosen_index in
      let rest = List.filter ((<>) chosen_item) items in
      chosen_item :: (shuffle_list rest (len - 1))
  in
  shuffle_list items (List.length items)

