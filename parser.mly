%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET MATRIX SEMI COMMA 
%token PLUS MINUS TIMES DIVIDE MOD MATMULT 
%token ASSIGN EQ NEQ NOT LT LEQ GT GEQ CONCAT
%token MATCH QMARK DONE CONT LOOP ANY TRUE PASS
%token NUM STRING FUNC SUB CONST INCLUDE POINT
%token EOF
%token <string>	LITNUM
%token <string> LITSTRING
%token <string>	ID

%right	ASSIGN
%left	EQ NEQ
%left	LT LEQ GT GEQ
%left	PLUS MINUS CONCAT
%left	TIMES DIVIDE MOD MATMULT
%right	NOT

start program
type <Ast.program> program

%%

program:
	(*nothing*)			{ [], [] }
	| program vdecl_stmt		{ ($2 :: fst $1), snd $1 }
	| program fdecl			{ fst $1, ($2 :: snd $1) }


fdecl:
	SUB ID LPAREN param_list_opt RPAREN func_stmt_list 
					{ {	fname = $2;
						params = $4;
						body = List.rev $6; } }

param_list_opt:
	(*nothing*)			{ [] }
	| param_list			{List.rev $1}	

param_list:
	vdecl				{ [] }
	| param_list COMMA vdecl 	{ $3 :: $1 }

vdecl_stmt:
	vdecl ASSIGN expr SEMI		{ Declinit($1, $3) }
	vdecl SEMI			{ Decl($1) }

vdecl:
	CONST var_type ID		{ {	vname = $3;
						vtype = $2;
						vmutable =  Const; } }
	| var_type ID			{ {	vname = $3;
						vtype = $2;
						vmutable =  Mutable; } }

var_type:
	NUM 				{ Num }
	| STRING 			{ String }
	| FUNC				{ Func }

func_stmt_list:
	stmt_list stmt			{ $2 :: $1 }
	stmt				{ [$1] }

stmt_list:
	(*nothing*)			{ [] }
	| stmt_list stmt		{ $2 :: $1 }

stmt:
	LBRACE stmt_list RBRACE		{ Block($2) }
	| MATCH LPAREN expr RPAREN LBRACE match_list RBRACE
					{ Match(
					     { 	match_expr = $3;
						match_list = List.rev $6 }
					) }
	| vdecl_stmt			{ Vdecl($1) }
	| expr				{ Expr($1) }
	| PASS SEMI			{ Pass }


match_list:
	(*nothing*)
	| match_list match_cmd  { $2 :: $1 }

match_command:
	flow_type match_cond QMARK stmt	{ {	f_type = $1;
						matchcmp = fst $2;
						match_expr = snd $2;
						match_stmt = $4; }}

flow_type:
	(*nothing*)			{ Cont }
	| CONT				{ Cont }
	| DONE				{ Done }
	| LOOP				{ Loop }

match_cond:
	match_cmp expr			{ ($1, $2) }
	| expr				{ (Eq, $2) }
	| TRUE				{ (Neq, 0) }
	| ANY				{ (Any, 0) }
	| DEFAULT			{ (Default, 0) }

match_cmp:
	NEQ				{ Neq }
	| LT				{ Lt }
	| LEQ				{ Leq }
	| GT				{ Gt }
	| GEQ				{ Geq }

(*Define Expression Here*)


