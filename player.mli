open Data;;

type player = {
  name : string;
  tokens : tokens;
  nobles : noble list;
  cards : card list;
  reserved: card list;
}

val empty_player : string -> player

val add_noble : noble -> player -> player
val add_card : card -> player -> player
val add_reserved_card : card -> player -> player
val add_token : token -> player -> player
val use_token : token -> player -> player

(* Should only be used during a purchase action,
 * in tandem with balancing tokens and adding the card back *)
val remove_reserved : int -> player -> player

val total_discount : player -> cost
val score_for_player : player -> int
val string_of_player : player -> string

