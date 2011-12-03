{ open Parser }

rule token = parse
	['' '\t' '\r' '\n'] { token lexbuf }
	| "\*"		{ comment lexbuf }

and comment = parse
	"*/"	{ token lexbuf }
	| _		{ comment lexbuf }