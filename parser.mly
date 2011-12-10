%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET LCSUB RCSUB PIPE
%token NEWMATRIX MATRIX NUMLIST STRLIST FUNLIST SEMI COMMA 
%token PLUS MINUS TIMES DIVIDE EXP MOD MATMULT FLOG FLN FCOS FSIN
%token ASSIGN EQ NEQ NOT LT LEQ GT GEQ CONCAT
%token MATCH QMARK DONE CONT LOOP ANY TRUE DEFAULT PASS
%token NUM STRING FUNC SUB CONST INCLUDE POINT
%token EOF
%token <string>	LITINT
%token <string>	LITFLOAT
%token <string> LITSTRING
%token <string>	ID

%right	ASSIGN
%left	EQ NEQ
%left	LT LEQ GT GEQ
%left	PLUS MINUS CONCAT
%left	TIMES DIVIDE MOD MATMULT
%right	EXP
%right	NOT

%start program
%type <Ast.program> program

%%

program:
	/*nothing*/			{ [], [] }
	| program vdecl_stmt		{ ($2 :: fst $1), snd $1 }
	| program fdecl			{ fst $1, ($2 :: snd $1) }

vdecl_stmt:
	vdecl ASSIGN expr SEMI		{ Declinit($1, $3) }
	| vdecl SEMI			{ Decl($1) }

vdecl:
	CONST var_type ID		{ {	vname = $3;
						vtype = $2;
						vmutable =  Const;} }
	| var_type ID			{ {	vname = $2;
						vtype = $1;
						vmutable =  Mutable; } }
var_type:
	basic_type			{ $1 }
	| var_type LBRACKET RBRACKET	{ (fst $1, snd $1 + 1) }

basic_type:
	NUM 				{ ( Num, 0 ) }
	| STRING 			{ ( String, 0) }
	| FUNC				{ ( Func, 0) }

fdecl:
	SUB ID LPAREN param_list_opt RPAREN LBRACE stmt_list RBRACE
					{ {	fname = $2;
						params = $4;
						body = List.rev $7; } }

param_list_opt:
	/*nothing*/			{ [] }
	| param_list			{List.rev $1}	

param_list:
	vdecl				{ [] }
	| param_list COMMA vdecl 	{ $3 :: $1 }

param_list_call_opt:
	/*nothing*/			{ [] }
	| param_list_call			{List.rev $1}	

param_list_call:
	expr				{ [] }
	| param_list_call COMMA expr 	{ $3 :: $1 }


stmt_list:
	/*nothing*/			{ [] }
	| stmt_list stmt		{ $2 :: $1 }

stmt:
	LBRACE stmt_list RBRACE		{ Block(List.rev $2) }
	| MATCH LPAREN expr RPAREN LBRACE match_list RBRACE
					{ Match(
					     { 	match_top_expr = $3;
						match_list = List.rev $6 }
					) }
	| vdecl_stmt SEMI			{ Vdecl($1) }
	| ID ASSIGN expr SEMI	{ Assign($1, $3) }
	| expr SEMI				{ Expr($1) }
	| PASS SEMI				{ Pass }

/*Define Expression Here*/
expr :
	  LITINT				{ Litnum($1) }
	| LITFLOAT				{ Litnum($1) }
	| LITSTRING				{ Litstring($1) }
	| PIPE func_param_list PIPE POINT PIPE func_expr PIPE
				 		{ Litfunc(List.rev $2, $6 ) }
	| ID					{ Id($1) }
	| expr PLUS expr			{ Binop($1, Add, $3) }
	| expr MINUS expr			{ Binop($1, Sub, $3) }
	| expr TIMES expr			{ Binop($1, Mult, $3) }
	| expr DIVIDE expr			{ Binop($1, Div, $3) }
	| expr EXP expr				{ Binop($1, Exp, $3) }
	| expr MOD expr				{ Binop($1, Mod, $3) }
	| expr MATMULT expr			{ Binop($1, MatMult, $3) }
	| expr EQ expr				{ Binop($1, Eq, $3) }
	| expr NEQ expr				{ Binop($1, Neq, $3) }
	| expr LT expr				{ Binop($1, Lt, $3) }
	| expr LEQ expr				{ Binop($1, Leq, $3) }
	| expr GT expr				{ Binop($1, Gt, $3) }
	| expr GEQ expr				{ Binop($1, Geq, $3) }
	| expr CONCAT expr			{ Binop($1, Concat, $3) }
	| MINUS expr				{ Unop(Uminus, $2) }
	| NOT expr				{ Unop(Not, $2) }
	| LPAREN expr RPAREN			{ $2 }
	| FLOG LPAREN param_list_call RPAREN	{ FCall(KeyFuncCall(Flog, $3))}
	| FLN LPAREN param_list_call RPAREN	{ FCall(KeyFuncCall(Fln, $3))}
	| FCOS LPAREN param_list_call RPAREN	{ FCall(KeyFuncCall(Fsin, $3))}
	| FSIN LPAREN param_list_call RPAREN	{ FCall(KeyFuncCall(Fcos, $3))}
	| ID LPAREN param_list_call RPAREN	{ FCall(FuncCall($1, $3))}
	| ID LCSUB param_list_call_opt RCSUB	{ Call($1, $3) }
	/*Put work for arrays and matrices here*/
	| basic_type LBRACE param_list_call RBRACE
						{ Newarr($1, List.rev $3) }
	| NUMLIST list_expr_list_opt RBRACE	{ Litarr(Num, List.rev $2)}
	| STRLIST list_expr_list_opt RBRACE	{ Litarr(String, List.rev $2)}
	| FUNLIST list_expr_list_opt RBRACE	{ Litarr(Func, List.rev $2)}

	| NEWMATRIX expr COMMA  expr RBRACKET
						{ Newmatrix($2, $4) }
	| MATRIX matrix_rows_list RBRACE	{ Litmatrix(List.rev $2) }

func_param_list:
	  ID				{ [$1] }
	| func_param_list COMMA ID	{ $3 :: $1 }
	
func_expr:
	  LITINT				{ Litnum($1) }
	| LITFLOAT				{ Litnum($1) }
	| ID					{ Id($1) }
	| func_expr PLUS func_expr		{ Binop($1, Add, $3) }
	| func_expr MINUS func_expr		{ Binop($1, Sub, $3) }
	| func_expr TIMES func_expr		{ Binop($1, Mult, $3) }
	| func_expr DIVIDE func_expr		{ Binop($1, Div, $3) }
	| func_expr EXP func_expr		{ Binop($1, Exp, $3) }
	| func_expr MOD func_expr		{ Binop($1, Mod, $3) }
	| func_expr MATMULT func_expr		{ Binop($1, MatMult, $3) }
	| func_expr EQ func_expr		{ Binop($1, Eq, $3) }
	| func_expr NEQ func_expr		{ Binop($1, Neq, $3) }
	| func_expr LT func_expr		{ Binop($1, Lt, $3) }
	| func_expr LEQ func_expr		{ Binop($1, Leq, $3) }
	| func_expr GT func_expr		{ Binop($1, Gt, $3) }
	| func_expr GEQ func_expr		{ Binop($1, Geq, $3) }
	| MINUS func_expr			{ Unop(Uminus, $2) }
	| NOT func_expr				{ Unop(Not, $2) }
	| LPAREN func_expr RPAREN		{ $2 }
	| FLOG LPAREN param_list_call RPAREN	{ FCall(KeyFuncCall(Flog, $3))}
	| FLN LPAREN param_list_call RPAREN	{ FCall(KeyFuncCall(Fln, $3))}
	| FCOS LPAREN param_list_call RPAREN	{ FCall(KeyFuncCall(Fsin, $3))}
	| FSIN LPAREN param_list_call RPAREN	{ FCall(KeyFuncCall(Fcos, $3))}
	| ID LPAREN param_list_call RPAREN	{ FCall(FuncCall($1, $3))}

list_expr_list_opt:
	  /*Nothing*/			{[]}
	| list_expr_list		{$1}

list_expr_list:
	  expr				{[$1]}
	| list_expr_list COMMA expr		{ $3 :: $1}

matrix_rows_list:
	  matrix_row_contents		{[List.rev $1]}
	| matrix_rows_list SEMI matrix_row_contents
					{ $3 :: $1 }

matrix_row_contents:
	  expr			{ [$1] }
	| matrix_row_contents COMMA expr
					{ $3 :: $1 }

match_list:
	/*nothing*/
	| match_list match_cmd  { $2 :: $1 }

match_cmd:
	flow_type match_cond QMARK stmt	{ {	f_type = $1;
						matchcmp = fst $2;
						match_expr = snd $2;
						match_stmt = $4; }}

flow_type:
	/*nothing*/			{ Cont }
	| CONT				{ Cont }
	| DONE				{ Done }
	| LOOP				{ Loop }

match_cond:
	match_cmp expr			{ ($1, $2) }
	| expr				{ (Eq, $1) }
	| TRUE				{ (Neq, 0) }
	| ANY				{ (Any, 0) }
	| DEFAULT			{ (Default, 0) }

match_cmp:
	NEQ				{ Neq }
	| LT				{ Lt }
	| LEQ				{ Leq }
	| GT				{ Gt }
	| GEQ				{ Geq }
