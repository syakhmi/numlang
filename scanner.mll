{ open Parser }

(*Guys, I am going to add some stuff in that seem to make sense. Lemme know if it looks wrong. - Dan*)

rule token = parse
	(* Parses as another token *)
	['' '\t' '\r' '\n'] { token lexbuf }
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
	
	(* Matrix Braces *)
	| "m["		{ MATRIX }

	(* Terminator: Semi-colon*)	
	| ';'		{ SEMI }

	(* Term Separator: Comma*)
	| ','		{ COMMA }

	(* Arithmatic Operations: Addition, Subtratction, Multiplicaiton, Division, Modulation, and Matrix Multiplication*)
	| '+'		{ PLUS }
	| '-'		{ MINUS }
	| '*'		{ TIMES }
	| '/'		{ DIVIDE }
	| '%'		{ MOD }
	| '#'		{ MATMULT }

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
	| "sub"		{ SUB }
	| "const"	{ CONST }

	(* Inculde statement*)
	| "include"	{ INCLUDE }


	(* Function mapping *)
	| "->"		{ POINT }

	(* End of File *)
	| eof		{ EOF }

	(* Need to put in literal int, literal float, literal string, and identifiers *)
	| ['0'-'9']+ '.' ['0'-'9']+ as lxm  { LITFLOAT(float_of_string lxm) }
	| ['0'-'9']+ as lxm { LITINT(int_of_string lxm) }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }

	(*All others*)
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
	"*/"	{ token lexbuf }
	| _	{ comment lexbuf }


(*I added in an part for single-line comment. You guys think we should do it? - Dan*)
and linecomment lexbuf = parse
	'\r'	{ token lexbuf }
	| '\n'	{ token lexbuf }
	| _	{ linecomment lexbuf}
