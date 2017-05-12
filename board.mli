open Globals
open Data

type 'a deck = {
  deck: 'a list;
  revealed: 'a list
}

type tier_level = One | Two | Three

val reveal_one : 'a deck -> 'a deck
val reveal : int -> 'a deck -> 'a deck

type board = {
  one: card deck;
  two: card deck;
  three: card deck;
  nobles: noble list;
  tokens: tokens
}

val tier_for_board : tier_level -> board -> card deck

val card_at : tier_level -> int -> board -> card option

(**
 * Remove the card of the specified tier and index from the board,
 * then return the removed card (if any) with the new board.
 * A new card will be revealed if necessary.
 *)
val claim_card_at : tier_level -> int -> board -> (card option * board)

val string_of_board : board -> string
