%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET MATRIX SEMI COMMA 
%token PLUS MINUS TIMES DIVIDE MOD MATMULT 
%token ASSIGN EQ NEQ NOT LT LEQ GT GEQ CONCAT
%token MATCH QMARK DONE CONT LOOP ANY TRUE PASS
%token NUM STRING FUNC SUB CONST INCLUDE POINT
%token EOF
%token <float>	LITFLOAT
%token <int>	LITINT
%token <string>	ID

%right	ASSIGN
%left	EQ NEQ
%left	LT LEQ GT GEQ
%left	PLUS MINUS CONCAT
%left	TIMES DIVIDE MOD MATMULT
%right	NOT


%%

program:
	(*nothing*)			{ [], [] }
	| program vdecl			{ ($2 :: fst $1), snd $1 }
	| program fdecl			{ (fst $1, ($2 :: snd $1) }


fdecl:
	SUB ID LPAREN param_list RPAREN stmt 
					{ {	fname = $2;
						params = $4;
						body = $6; } }

param_list_opt:
	(*nothing*)			{ [] }
	| param_list			{List.rev $1}	

param_list:
	vdecl				{ [$3] }
	| param_list COMMA vdecl 	{ $3 :: $1 }

vdecl:
	CONST var_type ID SEMI		{ ($3, $2, Const) }
	| var_type ID SEMI		{ ($3, $2, Mutable) }

var_type:
	NUM 				{ Num }
	| STRING 			{ String }
	| FUNC				{ Func }


stmt_list:
	(*nothing*)
	| stmt_list stmt

stmt:
	LBRACE stmt_list RBRACE
	| MATCH LPAREN expr RPAREN LBRACE match_list RBRACE
	| expr
	| PASS SEMI

match_list:
	(*nothing*)
	| match_list match_cmd

match_command:
	flow_type match_cond QMARK stmt	{ (

flow_type:
	(*nothing*)			{ Cont }
	| CONT				{ Cont }
	| DONE				{ Done }
	| LOOP				{ Loop }

match_cond:
	match_cmp expr			{ ($1, $2) }
	| expr				{ (Eq, $2) }
	| TRUE				{ (Neq, 0) }
	| ANY				{ (Eq, Any) }

match_cmp:
	NEQ				{ Neq }
	| LT				{ Lt }
	| LEQ				{ Leq }
	| GT				{ Gt }
	| GEQ				{ Geq }
