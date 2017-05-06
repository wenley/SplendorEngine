
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

let string_of_token (token:token) =
  match token with
  | Gold -> "Y"
  | Normal c -> string_of_color c

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

type cost = int ColorMap.t
type tokens = int TokenMap.t

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
type card = { score: int; color: color; cost: cost }

(* Testing code *)
