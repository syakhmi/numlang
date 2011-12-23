%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET LCSUB RCSUB PIPE
%token NEWMATRIX MATRIX LIST SEMI COMMA 
%token PLUS MINUS TIMES DIVIDE EXP MOD MATMULT FLOG FLN FCOS FSIN
%token ASSIGN EQ NEQ NOT LT LEQ GT GEQ CONCAT
%token MATCH QMARK DONE CONT LOOP ANY TRUE PASS
%token NUM STRING FUNC SUB CONST EXTERN INCLUDE POINT
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

/* Rules for a program */
program:
	rev_program		{ List.rev $1 }

rev_program:
	/*nothing*/				{ [] }
	| rev_program stmt			{ $2 :: $1 }

/* Rules for a subroutine in the program */
sdecl:
	SUB ID LPAREN param_list_opt RPAREN LBRACE stmt_list RBRACE
					{ Subdecl($2, List.rev $4, List.rev $7) }

/* Defines parameters to a subroutine and basic types*/
param_list_opt:
	/*nothing*/			{ [] }
	| param_list			{$1}	

param_list:
	var_type ID			{ [{	vname = $2;
						vtype = $1;
					  }] }
	| param_list COMMA var_type ID 	{ {	vname = $4;
						vtype = $3;
					  } :: $1 }

var_type:
	basic_type			{ $1 }
	| var_type LIST	{ List($1) }

basic_type:
	NUM 				{ Num }
	| STRING 			{ String }
	| FUNC				{ Func }
	| MATRIX			{ Matrix }

/* Rules for a statement in a program */
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
	| assign_stmt			{ $1 }
	| expr SEMI			{ Expr($1) }
	| PASS SEMI			{ Pass }
	| sdecl				{ $1 }

/* Rules for match statements */
match_list:
	/*nothing*/				{ [] }
	| match_list match_cmd  { $2 :: $1 }

match_cmd:
	flow_type match_cond QMARK stmt	{ {	f_type = $1;
						match_cmp = fst $2;
						match_expr = snd $2;
						match_stmt = $4; }}

flow_type:
	/*nothing*/			{ Cont }
	| CONT				{ Cont }
	| DONE				{ Done }
	| LOOP				{ Loop }

match_cond:
	match_cmp expr			{ ($1, $2) }
	| expr				{ (Meq, $1) }
	| TRUE				{ (Mneq, Litnum("0")) } 
	| ANY				{ (Any, Litnum("0")) }

match_cmp:
	NEQ				{ Mneq }
	| LT				{ Mlt }
	| LEQ				{ Mleq }
	| GT				{ Mgt }
	| GEQ				{ Mgeq }

assign_stmt:
	  ID list_access_opt ASSIGN expr SEMI	{ Assign($1,
						 $2, $4) }
	| CONST ID ASSIGN expr SEMI	
					{ Constassign($2, $4) }
	| EXTERN ID list_access_opt ASSIGN expr SEMI	{ Externassign($2,
							 $3, $5) }

/* Rules for an expression*/
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
	| FLOG LPAREN expr RPAREN	{ FCall(KeyFuncCall(Flog, $3))}
	| FLN LPAREN expr RPAREN	{ FCall(KeyFuncCall(Fln, $3))}
	| FCOS LPAREN expr RPAREN	{ FCall(KeyFuncCall(Fcos, $3))}
	| FSIN LPAREN expr RPAREN	{ FCall(KeyFuncCall(Fsin, $3))}
	| ID LPAREN param_list_call RPAREN	{ FCall(FuncCall($1, $3))}
	| ID LCSUB param_list_call_opt RPAREN	{ Call($1, $3) }
	| NUM LCSUB param_list_call_opt RPAREN	{ Call("num", $3)}
/*	| ID LCSUB param_list_call_opt RCSUB	{ Call($1, $3) }
	| NUM LCSUB param_list_call_opt RCSUB	{ Call("num", $3)}
*/	| ID list_access			{ Access($1, List.rev $2) }
	| LBRACKET list_expr_list_opt RBRACKET	{ Litlist(List.rev $2)}
	| MATRIX matrix_rows_list RBRACKET	{ Litmatrix(List.rev $2) }

list_access_opt:
	/* nothing */					{ [] }
	| list_access					{ $1 }

list_access:
	LBRACKET expr RBRACKET					{ [$2] }
	| list_access LBRACKET expr RBRACKET	{ $3 :: $1 }

func_param_list:
	  ID				{ [$1] }
	| func_param_list COMMA ID	{ $3 :: $1 }

/* Rules for a functional Expression*/
func_expr:
	  LITINT				{ FLitnum($1) }
	| LITFLOAT				{ FLitnum($1) }
	| ID					{ FId($1) }
	| func_expr PLUS func_expr		{ FBinop($1, FAdd, $3) }
	| func_expr MINUS func_expr		{ FBinop($1, FSub, $3) }
	| func_expr TIMES func_expr		{ FBinop($1, FMult, $3) }
	| func_expr DIVIDE func_expr		{ FBinop($1, FDiv, $3) }
	| func_expr EXP func_expr		{ FBinop($1, FExp, $3) }
	| func_expr MOD func_expr		{ FBinop($1, FMod, $3) }
	| func_expr EQ func_expr		{ FBinop($1, FEq, $3) }
	| func_expr NEQ func_expr		{ FBinop($1, FNeq, $3) }
	| func_expr LT func_expr		{ FBinop($1, FLt, $3) }
	| func_expr LEQ func_expr		{ FBinop($1, FLeq, $3) }
	| func_expr GT func_expr		{ FBinop($1, FGt, $3) }
	| func_expr GEQ func_expr		{ FBinop($1, FGeq, $3) }
	| MINUS func_expr			{ FUnop(Uminus, $2) }
	| NOT func_expr				{ FUnop(Not, $2) }
	| LPAREN func_expr RPAREN		{ $2 }
	| FLOG LPAREN func_expr RPAREN	{ FFCall(FKeyFuncCall(Flog, $3))}
	| FLN LPAREN func_expr RPAREN	{ FFCall(FKeyFuncCall(Fln, $3))}
	| FCOS LPAREN func_expr RPAREN	{ FFCall(FKeyFuncCall(Fcos, $3))}
	| FSIN LPAREN func_expr RPAREN	{ FFCall(FKeyFuncCall(Fsin, $3))}
	| ID LPAREN fparam_list_call RPAREN	{ FFCall(FFuncCall($1, $3))}

param_list_call_opt:
	/*nothing*/			{ [] }
	| param_list_call			{List.rev $1}	

param_list_call:
	expr				{ [$1] }
	| param_list_call COMMA expr 	{ $3 :: $1 }

fparam_list_call:
		func_expr				{ [$1] }
		| fparam_list_call COMMA func_expr 	{ $3 :: $1 }

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


