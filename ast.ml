type bop = Add | Sub | Mult | Div | Mod | MatMult | Eq | Neq | Lt | Leq | Gt 
	 | Geq | Concat
type uop = Uminus | Not
type cftype = Cont | Done | Loop
type matchcmptype = Eq | Neq | Lt | Leq | Gt | Geq | Any | Default
type vartype = Num | String | Func 
type mutab = Const | Mutable

type var_decl = {
	vname : string;
	vtype : vartype;
	vmutable : mutab;
}

type expr =
	  Litnum of string
	| Litstring of string
	| Litfunc of string list * expr
	| Id of var_decl
	| Binop of expr * bop * expr
	| Unop of uop * expr
	| Assign of string * expr
	| Call of string * expr list
	| Noexpr

type match_command = {
	f_type : cftype;
	match_cmp : matchcmptype;
	match_expr : expr;
	match_stmt : stmt;
}

type match_statement = {
	match_expr : expr;
	match_list : match_command list;
}

type vdecl_statement = 
	  Decl of var_decl
	| Declinit of var_decl * expr	

type stmt = 
	  Block of stmt list
	| Match of match_statement
	| Vdecl of vdecl_statement
	| Expr of expr
	| Pass

type func_decl = {
	fname : string;
	params : var_decl;list;
	body : stmt list;
}

type program = var_decl list * func_decl list (* global variables, functions*)
