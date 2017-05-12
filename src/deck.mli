(* Public interface for Deck type. *)

type deck
type card

exception EmptyDeck
exception InvalidCard

val empty_deck : deck;;
val none_card : card;;
val generate_deck : unit -> deck;;
val pop_card : deck -> card * deck;;
val split_deck : deck -> int -> deck * deck;;
val deck_size : deck -> int;;
val card_from_string : string -> card;;
val card_to_string : card -> string;;
val print_deck_size : int -> unit;;
val print_deck : deck -> unit;;
val valid_card : card -> bool;;
val is_special : card -> bool;;
val handle_special : card -> deck -> deck -> deck * deck;;
val add_card : card -> deck -> deck;;
val card_exists : card -> deck -> bool;;
val cards_equal : card -> card -> bool;;
val biggest_card : deck -> card;;
val biggest : card -> card -> card;;
val smallest : card -> card -> card;;
val remove_card : card -> deck -> deck;;
