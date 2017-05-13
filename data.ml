
type color = Black | Blue | Green | Red | White

type token = Normal of color | Gold

type score = int;;

let string_of_color (color:color) : string =
  match color with
  | Black -> "K"
  | Blue -> "B"
  | Green -> "G"
  | Red -> "R"
  | White -> "W"

let verbose_string_of_color (color : color) : string =
  match color with
  | Black -> "Black"
  | Blue -> "Blue"
  | Green -> "Green"
  | Red -> "Red"
  | White -> "White"

let string_of_token (token:token) =
  match token with
  | Gold -> "Y"
  | Normal c -> string_of_color c

let verbose_string_of_token (token:token) =
  match token with
  | Gold -> "Gold"
  | Normal c -> verbose_string_of_color c

module ColorOrder : (Map.OrderedType with type t = color) = struct
  type t = color
  let compare c1 c2 =
    String.compare (string_of_color c1) (string_of_color c2)
end

module TokenOrder : (Map.OrderedType with type t = token) = struct
  type t = token
  let compare t1 t2 =
    String.compare (string_of_token t1) (string_of_token t2)
end

module ColorMap : (Map.S with type key = color) = Map.Make(ColorOrder)
module TokenMap : (Map.S with type key = token) = Map.Make(TokenOrder)
module ColorCounter = Globals.Counter(ColorMap)
module TokenCounter = Globals.Counter(TokenMap)

type cost = int ColorMap.t
type tokens = int TokenMap.t

let verbose_string_of_cost (cost:cost) =
  let add_color (color:color) (count:int) s =
    let color_string = verbose_string_of_color color in
    Printf.sprintf "%d %s, %s" count color_string s
  in
  let message = ColorMap.fold add_color cost "" in
  Printf.sprintf "[%s]" message

let verbose_string_of_tokens (tokens:tokens) =
  let add_color (color:color) (count:int) s =
    let color_string = verbose_string_of_color color in
    Printf.sprintf "%d %s, %s" count color_string s
  in
  let add_token (token:token) (count:int) (s:string) : string =
    match token with
    | Normal c -> add_color c count s
    | Gold -> Printf.sprintf "%d %s, %s" count "Gold" s
  in
  let message = TokenMap.fold add_token tokens "" in
  Printf.sprintf "[%s]" message

let color_of_string (input : string) : color option =
  match input with
  | "K" -> Some (Black)
  | "B" -> Some (Blue)
  | "G" -> Some (Green)
  | "R" -> Some (Red)
  | "W" -> Some (White)
  | _ -> None

let token_of_string (input : string) : token option =
  match input with
  | "K" | "B" | "G" | "R" | "W" ->
      (match color_of_string input with
      | Some color -> Some (Normal (color))
      | None -> failwith "Unexpected None from color_of_string"
      )
  | "Y" -> Some (Gold)
  | _ -> None

type noble = { score: int; cost: cost }

let string_of_noble (noble:noble) : string =
  let { score; cost } = noble in
  Printf.sprintf "Noble: %d points %s" score (verbose_string_of_cost cost)

type card = { score: int; color: color; cost: cost }

let verbose_string_of_card (card:card) : string =
  let { score; color; cost } = card in
  Printf.sprintf
    "{ score: %d, color: %s; cost %s }"
    score
    (verbose_string_of_color color)
    (verbose_string_of_cost cost)
  ;;

(* Testing code *)
