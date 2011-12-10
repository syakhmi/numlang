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
	| "<<"		{ LCSUB }
	| ">>"		{ RCSUB }

	
	(* Matrix Braces *)
	| "m["		{ NEWMATRIX }
	| "m{"		{ MATRIX }

	(* Literla Lists *)
	| "n{"		{ NUMLIST }
	| "s{"		{ STRLIST }
	| "f{"		{ FUNLIST }

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
	| "default"	{ DEFAULT }
	
	(* Control Flow *)

	(* Null Statement*)
	| "pass"	{ PASS }

	(* Type Declaration: num, string, func, and sub, + const *)
	| "num"		{ NUM }
	| "string"	{ STRING }
	| "func"	{ FUNC }
	| "sub"		{ SUB }
	| "const"	{ CONST }

	(* Inculde statement*)
	| "include"	{ INCLUDE }


	(* Function mapping *)
	| "->"		{ POINT }

	(* End of File *)
	| eof		{ EOF }

	(* Float Literal *)
	| ['0'-'9']+ '.' ['0'-'9']+ as lxm  { LITFLOAT(lxm) }

	(* Int Literal *)
	| ['0'-'9']+ as lxm { LITINT(lxm) }

	(* String Literal *)
	| '"' {STRBEGIN; str lexbuf}
	| "'" {STRBEGIN; hardstr lexbuf}

	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }



	(* My attempt at doing strings *)
		(*All others*)
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }


(* Parses the characters in a hard string (no escape characters) *)
and hardstr = parse
	"'"	{STREND; token lexbuf}
	| _ as char {STRCHAR(char); str lexbuf}

(* Parses the characters of a string *)
and str = parse
	'"'	{STREND; token lexbuf}
	| '\\' {escapechar lexbuf} 
	| _ as char {STRCHAR(char); str lexbuf}

(* Handles Escape Characters in strings *)
and escapechar = parse
	't'	{ STRCHAR('\t') ; str lexbuf }
	| 'r'	{ STRCHAR('\r') ; str lexbuf }
	| 'n'	{ STRCHAR('\n') ; str lexbuf } 
	| '\\'	{ STRCHAR('\\') ; str lexbuf }
	| '"'	{ STRCHAR('"') ; str lexbuf }
	| _ as char { raise (Failure("illegal escape character " ^ Char.escaped char)) }

(* Handles Comments*)
and comment = parse
	"*/"	{ token lexbuf }
	| _	{ comment lexbuf }


(*I added in an part for single-line comment. You guys think we should do it? - Dan*)
and linecomment lexbuf = parse
	'\r'	{ token lexbuf }
	| '\n'	{ token lexbuf }
	| _	{ linecomment lexbuf}
