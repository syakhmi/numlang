type bop = Add | Sub | Mult | Div | Mod | MatMult | Eq | Neq | Lt | Leq | Gt 
	 | Geq | Concat
type uop = Uminus | Not
type cftype = Cont | Done | Loop
type vartype = Num | String | Func 
type mutab = Const | Mutable


type expr =
	  Litfloat of float
	| Litint of int
	| Litstring of string
	| Id of string * vartype * mutab
	| Binop of expr * bop * expr
	| Unop of uop * expr
	| Assign of string * expr
	| Call of string * expr list
	| Noexpr

