(* Main *)

open Game

let play () =
    try
        let game = start_game () in
        show_results game
    with
        | Failure explanation -> print_endline explanation
;;

play ()
