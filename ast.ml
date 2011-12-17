type fbop = FAdd | FSub | FMult | FDiv | FExp | FMod | FMatMult
	  | FEq | FNeq | FLt | FLeq | FGt | FGeq
type bop = 	Add | Sub | Mult | Div | Exp | Mod | MatMult
		  | Eq | Neq | Lt | Leq | Gt | Geq | Concat
type uop = Uminus | Not
type fkeyfuncs =  Flog | Fln | Fcos | Fsin
type cftype = Cont | Done | Loop
type matchcmptype = Meq | Mneq | Mlt | Mleq | Mgt | Mgeq | Any | Default
type mutab = Const | Mutable

type v_type =
	  None
	| Num 
	| String
	| Func
	| Matrix of int * int
	| Array of v_type * int

type func_call =
	  KeyFuncCall of fkeyfuncs * expr list
	| FuncCall of string * expr list

and func_expr =
	| FLitnum of string
	| FId of string
	| FBinop of func_expr * fbop * func_expr
	| FUnop of uop * func_expr
	| FFCall of func_call

and expr =
	  Litnum of string
	| Litstring of string
	| Litfunc of string list * func_expr
	| Litlist of v_type * expr list
	| Litmatrix of expr list list
	| Id of string
	| Ref of expr * expr
	| Slice of expr * expr * expr
	| Binop of expr * bop * expr
	| Unop of uop * expr
	| Call of string * expr list
	| FCall of func_call
	| Noexpr

and stmt =
	  Block of stmt list
	| Match of match_statement
	| Assign of string * expr list  * expr
	| Constassign of string * expr list * expr
	| Externassign of string * expr list * expr
	| Expr of expr
	| Pass
	| Subdecl of string * var_decl list * stmt list

and match_command = {
	f_type : cftype;
	match_cmp : matchcmptype;
	match_expr : expr;
	match_stmt : stmt;
}

and match_statement = {
	match_top_expr : expr;
	match_list : match_command list;
}

and var_decl = {
	vname : string;
	vtype : v_type;
}

type program = stmt list (* global variables, functions*)
