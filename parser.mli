type token =
  | FLOAT of (float)
  | INT of (int)
  | TRUE
  | FALSE
  | DBLSEMI
  | IF
  | THEN
  | ELSE
  | OR
  | AND
  | NOT
  | XOR
  | NAND
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | COMPOP of (string)
  | EQ
  | NEQ

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Types.exprS
