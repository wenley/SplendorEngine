
let (|>) x f = f x

module Lists = struct
  let rec first (items:'a list) (n:int) : 'a list =
    match items, n with
    | ([], 0) -> []
    | (_, 0) -> []
    | ([], _) -> []
    | (hd::tl, n) -> hd :: (first tl (n-1))
end

let string_of_list (string_of:'a -> string) (joiner:string) (items:'a list) : string =
  let strings = List.map string_of items in
  let join (msg:string) (next:string) : string =
    msg ^ joiner ^ next
  in
  List.fold_left join "" strings

