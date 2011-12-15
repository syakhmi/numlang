open Ast

(*type t =
	  Scalar of Ast.vartype
	| List of t * int
	| Matrix of int * int

type t_expr = t * expr*)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let expr = Scanner.token lexbuf in
	Printf.printf expr