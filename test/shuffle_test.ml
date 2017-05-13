open Shuffle;;

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
