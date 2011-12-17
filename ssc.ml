open Ast

type symbol_table = {
	parent : symbol_table_option;
	variables : varialble_decl list
}

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.program Scanner.token lexbuf in
	Printf.printf "done\n"