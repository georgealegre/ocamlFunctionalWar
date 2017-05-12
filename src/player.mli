open Deck

type player
type players

exception NotEnoughPlayers
exception DoneLoading
exception Exit

val empty_players : players;;
val get_players : unit -> players;;
val num_players : players -> int;;
val replace_deck : player -> deck -> player;;
val add_player : player -> players -> players;;
val deal_hand : players -> deck -> players * deck;;
val filter_players : (player -> bool) -> players -> players;;
val played_card : player -> card;;
val get_deck : player -> deck;;
val score : player -> int;;
val name : player -> string;;
val print_played_cards : players -> unit;;
val print_name_score_in_hand : player -> unit;;
val p_remove_card : card -> player -> card * player;;
val p_handle_special : card -> deck -> player -> deck * player;;
val p_handle_normal : card -> deck -> player -> deck * player;;
val handle_players_after_round : players -> players;;
val player_has_card : card -> player -> bool;;
val fold_over_players : (players * deck -> player -> players * deck) ->
                        players * deck -> players -> players * deck;;
val iteri_over_players : (int -> player -> unit) -> players -> unit;;
val give_card : card -> player -> player;;
val score_sort : players -> players;;
