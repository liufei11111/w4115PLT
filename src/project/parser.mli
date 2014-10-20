type token =
  | SEMI
  | COLON
  | LPAREN
  | RPAREN
  | LSQUARE
  | RSQUARE
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | MPLUS
  | MMINUS
  | MTIMES
  | TIMES
  | DIVIDE
  | MDIVIDE
  | ASSIGN
  | ARROW
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | BOOLEAN
  | TRUE
  | FALSE
  | MATRIX
  | STRUCTURE
  | OPTION
  | INT
  | FLOAT
  | STRING
  | VOID
  | INT_LIT of (int)
  | FLOAT_LIT of (float)
  | STRING_LIT of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
