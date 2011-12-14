open Ast

type t =
	  Scalar of vartype
	| List of t * int
	| Matrix of int * int

type t_expr = t * expr
