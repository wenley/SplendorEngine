
let (|>) x f = f x

module Lists = struct
  let rec first (items:'a list) (n:int) : 'a list =
    match items, n with
    | ([], 0) -> []
    | (_, 0) -> []
    | ([], _) -> []
    | (hd::tl, n) -> hd :: (first tl (n-1))
end

module Option = struct
  let flat_map (f:'a -> 'b option) (value: 'a option) : 'b option=
    match value with
    | Some v -> f v
    | None -> None

  let map (f:'a -> 'b) (value: 'a option) : 'b option =
    match value with
    | Some v -> Some (f v)
    | None -> None
end

module Counter (MapType:Map.S) : sig
  val increment : MapType.key -> int MapType.t -> int MapType.t
  val decrement : MapType.key -> int MapType.t -> int MapType.t
end = struct
  let apply_delta (value:int) (key:MapType.key) (map:int MapType.t) : int MapType.t =
    let has_key = MapType.exists (fun k -> fun _ -> k = key) map in
    match has_key with
    | false -> MapType.add key value map
    | true ->
        let old_value = MapType.find key map in
        MapType.add key (old_value+value) map

  let increment = apply_delta 1

  let decrement key map =
    let positive_value (_:MapType.key) (value:int) : bool =
      value > 0
    in
    apply_delta (-1) key map |> MapType.filter positive_value
end

let string_of_list (string_of:'a -> string) (joiner:string) (items:'a list) : string =
  let strings = List.map string_of items in
  let join (msg:string) (next:string) : string =
    msg ^ joiner ^ next
  in
  List.fold_left join "" strings

