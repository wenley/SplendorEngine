open Start;;

(**
 * This doesn't work via Rakefile!
 * Rakefile needs to stream Stdout instead of collecting the final string
 *)
let game = RealStart.start_game () in
let { Game.board } = game in
print_string (Board.string_of_board board) ; print_newline ()
