open Data;;

type player = {
  name : string;
  tokens : tokens;
  nobles : noble list;
  cards : card list;
  reserved: card list;
}

val empty_player : string -> player
val reserve_card_at : int -> player -> Data.card option

(* Should only be used during a purchase action,
 * in tandem with balancing tokens and adding the card back *)
val remove_reserved : int -> player -> (Data.card option * player)

val total_discount : player -> cost
val score_for_player : player -> int
val string_of_player : player -> string
val print_player : player -> unit

