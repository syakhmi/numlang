{ open Parser }

rule token = parse
	(* Parses as another token *)
	[' ' '\t' '\r' '\n'] { token lexbuf }
	(* Starts treating text as comments *)
	| "/*"		{ comment lexbuf }
	(* Starts treating text as a line comment *)
	| "//"		{linecomment lexbuf}

	(*Grouping: Parenthesis, Braces, and Brackets*)
	| '('		{ LPAREN }
	| ')'		{ RPAREN }
	| '{'		{ LBRACE }
	| '}'		{ RBRACE }
	| '['		{ LBRACKET }
	| ']'		{ RBRACKET }
	| '|'		{ PIPE }
	| "::("		{ LCSUB }
	| ">>"		{ RCSUB }

	
	(* Matrix Begin Bracket *)
	| "m["		{ MATRIX }

	(*(* Literla Lists *)
	| "n{"		{ NUMLIST }
	| "s{"		{ STRLIST }
	| "f{"		{ FUNLIST }*)

	(* Terminator: Semi-colon*)	
	| ';'		{ SEMI }

	(* Term Separator: Comma*)
	| ','		{ COMMA }

	(* Arithmatic Operations: Addition, Subtratction, Multiplicaiton, Division, Modulation, and Matrix Multiplication*)
	| '+'		{ PLUS }
	| '-'		{ MINUS }
	| '*'		{ TIMES }
	| '/'		{ DIVIDE }
	| '^'		{ EXP }
	| '%'		{ MOD }
	| '#'		{ MATMULT }

	(* Special Functions For Functions *)
	| "log"		{ FLOG }
	| "ln"		{ FLN }
	| "cos"		{ FCOS }
	| "sin"		{ FSIN }

	(* Assignment *)
	| '='		{ ASSIGN }

	(* Logic: Equals, Not Equals, Negation, Less Than, Less than or Equal, Greater Than, and Greater Than or Equal *)
	| "=="		{ EQ }
	| "!="		{ NEQ }
	| '!'		{ NOT }
	| '<'		{ LT }
	| "<="		{ LEQ }
	| '>'		{ GT }
	| ">="		{ GEQ }

	(* Concatenation *)
	| "."		{ CONCAT }

	(* Control Flow *)
	| "match"	{ MATCH }
	| '?'		{ QMARK }

	(* Control Flow Actions: done, continue, and loop*)
	| "done:"	{ DONE }
	| "cont:"	{ CONT }
	| "loop:"	{ LOOP }

	(* Control Flow Match Conditions: Any, True *)
	| "any"		{ ANY }
	| "true"	{ TRUE }
	
	(* Control Flow *)

	(* Null Statement*)
	| "pass"	{ PASS }

	(* Type Declaration: num, string, func, and sub, + const *)
	| "num"		{ NUM }
	| "string"	{ STRING }
	| "func"	{ FUNC }
	| "list"	{ LIST }
	| "sub"		{ SUB }
	| "const"	{ CONST }
	| "extern"	{ EXTERN }

	(* Inculde statement*)
	| "include"	{ INCLUDE }


	(* Function mapping *)
	| '-''>'		{ POINT }

	(* End of File *)
	| eof		{ EOF }

	(* Float Literal *)
	| ['0'-'9']+ '.' ['0'-'9']+ as lxm  { LITFLOAT(lxm) }

	(* Int Literal *)
	| ['0'-'9']+ as lxm { LITINT(lxm) }

	(* String Literal *)
	| '"'((([^'"''\\'])|('\\''t')|('\\''r')|('\\''n')|('\\''\\')|('\\''"'))* as s)'"'	{ LITSTRING(s) }

	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

(* Handles Comments*)
and comment = parse
	"*/"	{ token lexbuf }
	| _	{ comment lexbuf }

(*I added in an part for single-line comment. You guys think we should do it? - Dan*)
and linecomment = parse
	'\r'	{ token lexbuf }
	| '\n'	{ token lexbuf }
	| _	{ linecomment lexbuf}
