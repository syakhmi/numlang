type fbop = Add | Sub | Mult | Div | Exp | Mod | MatMult
	  | Eq | Neq | Lt | Leq | Gt | Geq
type bop = fbop | Concat
type uop = Uminus | Not
type fkeyfuncs =  Flog | Fln | Fcos | Fsin
type cftype = Cont | Done | Loop
type matchcmptype = Eq | Neq | Lt | Leq | Gt | Geq | Any | Default
type vartype = Num | String | Func 
type mutab = Const | Mutable

type func_call =
	  KeyFuncCall of fkeyfuncs * string list
	| FuncCall of string * string list

type func_expr =
	| Litnum of string
	| Id of string
	| Fbinop of func_expr * fbop * func_expr
	| Funop of unop * func_expr
	| FCall of func_call

type expr =
	  Litnum of string
	| Litstring of string
	| Litfunc of string list * func_expr
	| Id of string
	| Binop of expr * bop * expr
	| Unop of uop * expr
	| Assign of string * expr
	| Call of string * expr list
	| FCall of func_call
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

type var_decl = {
	vname : string;
	vtype : vartype * int;
	vmutable : mutab;
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

type program = vdecl_stmt list * func_decl list (* global variables, functions*)
