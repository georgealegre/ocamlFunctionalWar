(* Game type implementation. *)

open Deck
open Player
open Printf

exception GameSetupError

type game = {players: players; deck: deck};;

let setup () =
    let initial_deck = generate_deck () in
    let initial_players =
        try
            get_players ()
        with
            | NotEnoughPlayers -> raise GameSetupError
    in
    let players, deck = deal_hand initial_players initial_deck in
    {players=players; deck=deck}
;;

let round_is_possible game =
    if deck_size game.deck = 0
    then (
        if num_players (filter_players (fun p -> (deck_size (get_deck p)) > 0) game.players) > 1
        then true
        else false
    ) else true
;;

(* Tengo que recorrer cada jugador mostrandole datos del mazo, las
 * cartas jugadas hasta el momento, su puntaje, sus cartas y leer que carta
 * va a jugar.
 * Primero, se le muestra esa info al jugador.
 * Se le pide que juege una carta, y luego se le da una del mazo.
 * Si es especial, se la maneja de manera apropiada. Luego, se le vuelve a mostrar la info.
 * Si es normal, se la guarda en la lista de cartas jugadas y se la guarda en el jugador.
 * Luego, me queda el mazo resultante y la lista de jugadores,
 * cada uno con la carta que jugo.
 * Luego, se extrae la mejor carta jugada.
 * Con esa carta, recorro jugadores hasta encontrarlo, eliminando carta jugada,
 * y le subo un punto.
 *)
let round game =
    let show_info ncards players player =
		print_newline ();
        print_endline "+------------------------------------------+";
        print_deck_size ncards; print_endline "+";
        print_played_cards players; print_endline "+";
        print_name_score_in_hand player;
        print_endline "+------------------------------------------+";
    in
    let rec read_card player =
        try
            let _ = printf "\nQue carta vas a jugar %s?\n" (name player) in
            let card = card_from_string (read_line ()) in
            if player_has_card card player
            then card
            else let _ = printf "No posee esa carta. Intente de nuevo: \n" in read_card player
        with
            | InvalidCard ->	let _ = printf "Carta inválida. Intente de nuevo: \n" in
                                read_card player
    in
    let rec turn (ps, deck) p =
        (* Remove top from deck if possible and add it to p's deck. *)
        let top, rest = pop_card deck in
        let player = give_card top p in

        (* Check if he can play. *)
		if deck_size (get_deck player) == 0
		then (
			let _ = printf "\nEl jugador %s no tiene más cartas.\n" (name player) in
            (* Don't lose the player. Still need to show his stats. *)
			add_player player ps, rest
			)
		else (
            (* Show info about the deck, others played cards and my own deck. *)
			show_info (deck_size rest) ps player;
            (* Get a card from me. *)
			let card, p_without_card = p_remove_card (read_card player) player in
			if is_special card
			then (
				let new_deck, new_player = p_handle_special card rest p_without_card in
				turn (ps, new_deck) new_player
				)
			else (
				let new_deck, new_player = p_handle_normal card rest p_without_card in
				add_player new_player ps, new_deck
				)
			)
	in
	let final_players, final_deck =
		fold_over_players turn (empty_players, game.deck) game.players
	in
	{players=handle_players_after_round final_players; deck=final_deck}
;;

let start_game () =
    let game =
        try
            printf "Bienvenidos a la Guerra Funcional. Ingrese jugadores:\n\n"; setup ()
        with
            | GameSetupError -> failwith "Insuficientes jugadores."
    in
    let rec play game =
        if round_is_possible game
        then play (round game)
        else game
    in
    play game
;;

let show_results game =
    let print_score i p =
        printf "\t%d %s\t\t %d\n" (i+1) (name p) (score p)
    in
    printf "\n\n\nGAME OVER. Posiciones:\n";
    iteri_over_players print_score (score_sort game.players)
;;
