open Ast

type t =
	  Scalar of vartype
	| Array of t * int
	| Matrix of int * int

type t_expr = t * expr
