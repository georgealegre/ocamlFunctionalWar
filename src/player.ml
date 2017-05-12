(* Player type implementation. *)

exception NotEnoughPlayers
exception Exit
exception DoneLoading

open List
open Deck
open Printf

type player = {name: string; score: int; in_hand: deck; card_played: card}
type players = player list

let min_players = 2;;
let max_players = 5;;
(* Cards per player. *)
let cpp = 7;;

let empty_players = ([]:players);;

(* TODO: Solucionar problema si usuario ingresa string vacio o string ya existente. *)
let get_players () =
    let create_player name =
        {name=name; score=0; in_hand=empty_deck; card_played=none_card}
    in
    let detect_exit input = match input with
        | "EXIT" -> raise Exit
        | _ -> create_player input
    in
    let s = "Ingrese el nombre del jugador o EXIT para comenzar el juego:" in
    let read_player () =
        try
            print_endline s; detect_exit (read_line ())
        with
            | Exit -> raise DoneLoading
    in
    let rec load n players = match n with
        | 0 -> (if length players > 1 then players
                else raise NotEnoughPlayers)
        | n -> (try
                    load (n-1) (read_player () :: players)
                with
                    | DoneLoading ->    if length players > 1 then players
                                        else raise NotEnoughPlayers)
    in
    load max_players empty_players
;;

let num_players players = length players;;

let replace_deck p deck =
    {name=p.name; score=p.score; in_hand=deck; card_played=p.card_played}
;;

let add_player player players = player :: players;;

let deal_hand players deck =
    let give_deck (players, deck) player =
        let h, t = split_deck deck cpp in
        add_player (replace_deck player h) players, t
    in
    fold_left give_deck (empty_players, deck) players
;;

let filter_players f players = filter f players;;

let get_deck player = player.in_hand;;

let name player = player.name;;

let score player = player.score;;

let played_card player = player.card_played;;

let print_played_cards players = print_endline "+\tRonda:";
    iter (fun p ->  if valid_card p.card_played
                    then printf "+\t\t%s\t%s\n" p.name (card_to_string p.card_played))
                    players
;;

let print_name_score_in_hand player =
    printf "+\t%s(%d): " player.name player.score;
    print_deck player.in_hand;
    printf "\n"
;;

let p_remove_card card player =
    let new_player = {
        name = player.name;
        score = player.score;
        in_hand = remove_card card player.in_hand;
        card_played = player.card_played}
    in
    card, new_player
;;

let give_card card p = {
    name = p.name;
    score = p.score;
    in_hand = add_card card p.in_hand;
    card_played = p.card_played}
;;

(* Handle special card. *)
let p_handle_special card deck player =
    let new_game_deck, new_player_deck = handle_special card deck player.in_hand in
    new_game_deck, (replace_deck player new_player_deck)
;;

let assign_card card p = {
    name = p.name;
    score = p.score;
    in_hand = p.in_hand;
    card_played = card}
;;

(* Handle normal card. *)
let p_handle_normal card deck player =
    deck, (assign_card card player)
;;

(* Check which player won, add point and clear played card. *)
let handle_players_after_round players =
    let (cards_played:deck) =
        fold_left (fun d p -> add_card p.card_played d) empty_deck players
    in
    let max = biggest_card cards_played in
    let add_point p = printf "\nEl jugador %s gano la ronda.\n" p.name;
        {name = p.name;
        score = (p.score + 1);
        in_hand = p.in_hand;
        card_played = none_card}
    in
    let reset p = {
        name = p.name;
        score = p.score;
        in_hand = p.in_hand;
        card_played = none_card}
    in
    let clear_add_point p = 
        if (cards_equal p.card_played max)
        then add_point p
        else reset p
    in
    let top_player (card, players) player =
        if (cards_equal player.card_played max)
        then max, player :: players
        else max, players
    in
    let _, top_players = fold_left top_player (max, []) players in
    if length top_players > 1
    then map reset players
    else map clear_add_point players
;;

let player_has_card card player =
    card_exists card player.in_hand
;;

let fold_over_players func (acc_players, deck) players =
    let new_players, new_deck = fold_left func (acc_players, deck) players in
    rev new_players, new_deck
;;

let iteri_over_players func players =
    iteri func players
;;

let score_sort players =
	let best p1 p2 =
		if p1.score == p2.score then 0
		else if p1.score < p2.score then +1
		else -1
	in
	sort best players
;;
