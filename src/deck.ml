(* Deck type implementation. *)

open List
open Printf

exception EmptyDeck
exception InvalidCard

type suit = Espada | Basto | Oro | Copa
type normal = {value: int; suit: suit}
type special = Id | Swap | Max | Min | Top | Par | Tpar | Sume | Mas1
type card = Normal of normal | Special of special | None
type deck = card list

let ncards_suit = 12;;
let min_deck_shuffle = 100;;
let rand_bound = 10000;;

let empty_deck = ([]:deck);;

let none_card = None;;

let rec gen_suit suit n = match n with
    | 0 -> empty_deck
    | n -> Normal {value = n; suit = suit} :: gen_suit suit (n - 1)
;;

let card_is_even card = match card with
    | Normal card -> if (card.value mod 2) == 0 then true else false
    | _ -> false
;;

let rec interweave deck1 deck2 = match deck1 with
    | [] -> deck2
    | c1 :: d1 -> (match deck2 with
        | [] -> deck1
        | c2 :: d2 -> c1 :: c2 :: (interweave d1 d2))
;;

let split_deck deck n =
    let rec rec_split i acc = function
        | [] -> rev acc, empty_deck
        | h :: t as ls -> if i = 0 then rev acc, ls
                          else rec_split (i-1) (h :: acc) t in
    rec_split n [] deck
;;

let rec rec_shuffle_deck deck n =
    let rand_split = Random.self_init(); Random.int (length deck) in
    let deck1, deck2 = split_deck deck rand_split in
    match deck with
        | [] -> empty_deck
        | h :: t as ls -> (match n with
            | 0 -> ls
            | n -> rec_shuffle_deck (interweave deck1 deck2) (n-1))
;;

let shuffle_deck deck =
    let rand = Random.self_init(); (Random.int rand_bound) + min_deck_shuffle in
    rec_shuffle_deck deck rand
;;

let generate_deck () =
    let deck = (Special Id :: Special Swap :: Special Max ::
                Special Min :: Special Top :: Special Par ::
                Special Sume :: Special Tpar :: Special Mas1 ::
                (gen_suit Espada 12) @ (gen_suit Basto 12) @
                (gen_suit Oro 12) @ (gen_suit Copa 12)) in
    shuffle_deck deck
;;

let deck_size deck = length deck
;;

let pop_card deck = match deck with
    | [] -> none_card, deck
    | h :: t -> h, t
;;

let card_from_string str =
    if String.length str < 2 then raise InvalidCard else (
        let h, t = (String.sub str 0 1), (String.sub str 1 ((String.length str) - 1)) in
        match h with
            | "E" -> Normal {value = int_of_string t; suit = Espada}
            | "C" -> Normal {value = int_of_string t; suit = Copa}
            | "O" -> Normal {value = int_of_string t; suit = Oro}
            | "B" -> Normal {value = int_of_string t; suit = Basto}
            | "S" -> (match t with
                | "ID" -> Special Id
                | "TOP" -> Special Top
                | "MAX" -> Special Max
                | "MIN" -> Special Min
                | "SWAP" -> Special Swap
                | "PAR" -> Special Par
                | "TPAR" -> Special Tpar
                | "SUME" -> Special Sume
                | "MAS1" -> Special Mas1 
                | _ -> raise InvalidCard)
            | _ -> raise InvalidCard
    )
;;

let print_deck_size size = print_string "+\tMazo: "; match size with
    | 0 -> print_endline "vacio."
    | 1 -> print_endline "1 carta."
    | n -> printf "%d cartas.\n" n
;;

let card_to_string card = match card with
    | Normal {value=value; suit=suit} -> (match suit with
        | Espada -> "E" ^ (string_of_int value)
        | Basto -> "B" ^ (string_of_int value)
        | Oro -> "O" ^ (string_of_int value)
        | Copa -> "C" ^ (string_of_int value))
    | Special card -> (match card with
        | Id -> "SID"
        | Swap -> "SSWAP"
        | Max -> "SMAX"
        | Min -> "SMIN"
        | Top -> "STOP"
        | Mas1 -> "SMAS1"
        | Sume -> "SSUME"
        | Tpar -> "STPAR"
        | Par -> "SPAR")
    | None -> "NONE"
;;

let print_deck deck =
    iter (fun c -> printf "%s " (card_to_string c)) deck
;;

let valid_card card = match card with
    | None -> false
    | _ -> true
;;

let is_none card = match card with
    | None -> true
    | _ -> false
;;

let is_normal card = match card with
    | Normal card -> true
    | _ -> false
;;

let is_special card = match card with
    | Special x -> true
    | _ -> false
;;

let add_card card deck = match card with
    | None -> deck
    | card -> card :: deck
;;

let biggest card1 card2 = match card1 with
    | None -> card2 
    | Special _ -> card1
    | Normal c1 -> (match card2 with
        | None -> card1
        | Special _ -> card2
        | Normal c2 -> (
            if c1.value < c2.value then card2
            else if c1.value > c2.value then card1
            else (match c1.suit with
                | Espada -> card1
                | Basto -> (match c2.suit with
                    | Espada -> card2
                    | _ -> card1)
                | Oro -> (match c2.suit with
                    | Espada -> card2
                    | Basto -> card2
                    | _ -> card1)
                | Copa -> card2
            )
        )
    )
;;

let smallest card1 card2 =
    if biggest card1 card2 = card1 then card2 else card1
;;

let is_tpar_valid card = match card with
    | Normal _ -> card_is_even card
    | _ -> true
;;

let normal_card_value card = match card with
    | Normal c -> c.value
    | _ -> 0
;;

let handle_special (card:card) (deck:deck) (player_deck:deck) = match deck with
(* ya se que estoy duplicando muchisimo el codigo aca,
 * las cartas especiales nuevas no me estaban andando a veces y me di cuenta
 * que era cuando el mazo estaba vacio y alguien las jugaba.
 * no tendria que haber hecho pattern matching con deck sino con card pero
 * bue, rindo en 2 dias y no tengo tiempo de sobra como para programar
 * de manera elegante.
 *)
	| [] -> (match card with
        | Special Swap -> player_deck, deck
        | Special Min -> (
            let top, rest = pop_card player_deck in
			let min = fold_left smallest top rest in
			let deck_with_min, remaining_deck =
				partition (fun c -> if c = min then true else false) player_deck
			in
			deck_with_min, remaining_deck
            )
        | Special Tpar -> (
            if for_all is_tpar_valid player_deck
            then deck, Normal {suit = Espada; value = 12} :: player_deck
            else deck, player_deck
            )
        | Special Sume -> (
            let value_normals total card = total + (normal_card_value card) in
            let total = fold_left value_normals 0 player_deck in
            if total > 0
            then deck, Normal {suit = Espada; value = total} :: player_deck
            else deck, player_deck
            )
        | Special Mas1 -> (
            let add_one card = match card with
                | Normal c -> Normal {suit = c.suit; value = c.value + 1}
                | _ -> card
            in
            deck, map add_one player_deck
            )
        | _ -> deck, player_deck
        )
	| top :: rest -> match card with
		| Special Id -> deck, player_deck
		| Special Swap -> player_deck, deck
		| Special Top -> rest, top :: player_deck
		| Special Max -> (
			let max = fold_left biggest top rest in
			let deck_with_max, remaining_deck =
				partition (fun c -> if c = max then true else false) deck
			in
			remaining_deck, (hd deck_with_max) :: player_deck
            )
		| Special Min -> (
            let topp, restp = pop_card player_deck in
			let min = fold_left smallest topp restp in
			let deck_with_min, remaining_deck =
				partition (fun c -> if c = min then true else false) player_deck
			in
			deck @ deck_with_min, remaining_deck
            )
		| Special Par -> (
			let deck_with_even, remaining_deck =
				partition card_is_even deck
			in
			remaining_deck, player_deck @ deck_with_even
			)
        | Special Tpar -> (
            if for_all is_tpar_valid player_deck
            then deck, Normal {suit = Espada; value = 12} :: player_deck
            else deck, player_deck
            )
        | Special Sume -> (
            let value_normals total card = total + (normal_card_value card) in
            let total = fold_left value_normals 0 player_deck in
            if total > 0
            then deck, Normal {suit = Espada; value = total} :: player_deck
            else deck, player_deck
            )
        | Special Mas1 -> (
            let add_one card = match card with
                | Normal c -> Normal {suit = c.suit; value = c.value + 1}
                | _ -> card
            in
            deck, map add_one player_deck
            )
		| _ -> deck, player_deck
;;

let biggest_card deck =
    fold_left biggest None deck
;;

let cards_equal card1 card2 = match card1 with
    | None -> (match card2 with
        | None -> true
        | _ -> false)
    | Special c1 -> (match card2 with
        | Special c2 -> c1 = c2
        | _ -> false)
    | Normal c1 -> (match card2 with
        | Normal c2 -> c1.suit = c2.suit && c1.value = c2.value
        | _ -> false)
;;

let card_exists card deck =
    exists (fun c -> cards_equal c card) deck
;;

let remove_card card deck =
    filter (fun c -> not (cards_equal c card)) deck
;;
