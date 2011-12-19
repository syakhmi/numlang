type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | LCSUB
  | RCSUB
  | PIPE
  | NEWMATRIX
  | MATRIX
  | NUMLIST
  | STRLIST
  | FUNLIST
  | SEMI
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EXP
  | MOD
  | MATMULT
  | FLOG
  | FLN
  | FCOS
  | FSIN
  | ASSIGN
  | EQ
  | NEQ
  | NOT
  | LT
  | LEQ
  | GT
  | GEQ
  | CONCAT
  | MATCH
  | QMARK
  | DONE
  | CONT
  | LOOP
  | ANY
  | TRUE
  | DEFAULT
  | PASS
  | NUM
  | STRING
  | FUNC
  | SUB
  | CONST
  | EXTERN
  | INCLUDE
  | POINT
  | EOF
  | LITINT of (string)
  | LITFLOAT of (string)
  | LITSTRING of (string)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
