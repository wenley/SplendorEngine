
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

(* Testing code *)
(*
let rec try_seeds (seed:int) : unit =
  if seed <= 0 then ()
  else
    ((*Printf.printf "Seed %d = " seed ; *)
    Random.init seed ;
    let result = shuffle (1 :: 2 :: 3 :: []) in
    List.iter print_int result ;
    print_newline () ;
    try_seeds (seed - 1))
in try_seeds 10000
;;
*)
