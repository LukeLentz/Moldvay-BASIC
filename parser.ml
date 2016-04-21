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
  | PLUSF
  | MINUSF
  | TIMESF
  | DIVIDEF
  | COMPOP of (string)
  | VARIABLE of (string)
  | EQ
  | NEQ
  | LET
  | BE
  | IN

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Types
# 36 "parser.ml"
let yytransl_const = [|
  259 (* TRUE *);
  260 (* FALSE *);
  261 (* DBLSEMI *);
  262 (* IF *);
  263 (* THEN *);
  264 (* ELSE *);
  265 (* OR *);
  266 (* AND *);
  267 (* NOT *);
  268 (* XOR *);
  269 (* NAND *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* TIMES *);
  273 (* DIVIDE *);
  274 (* PLUSF *);
  275 (* MINUSF *);
  276 (* TIMESF *);
  277 (* DIVIDEF *);
  280 (* EQ *);
  281 (* NEQ *);
  282 (* LET *);
  283 (* BE *);
  284 (* IN *);
    0|]

let yytransl_block = [|
  257 (* FLOAT *);
  258 (* INT *);
  278 (* COMPOP *);
  279 (* VARIABLE *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\006\000\003\000\
\003\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\004\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\003\000\004\000\005\000\006\000\000\000\000\000\
\000\000\025\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\017\000\
\019\000\000\000\000\000\018\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\010\000\011\000\012\000"

let yysindex = "\030\000\
\026\255\000\000\000\000\000\000\000\000\000\000\026\255\026\255\
\010\255\000\000\031\255\209\255\174\255\233\255\014\255\000\000\
\026\255\026\255\026\255\026\255\026\255\026\255\026\255\026\255\
\026\255\026\255\026\255\026\255\026\255\026\255\026\255\026\255\
\026\255\221\255\221\255\221\255\221\255\018\255\018\255\000\000\
\000\000\018\255\018\255\000\000\000\000\244\254\233\255\233\255\
\192\255\244\254\026\255\209\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\040\255\000\000\146\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\251\255\004\000\013\000\022\000\041\255\062\255\000\000\
\000\000\083\255\104\255\000\000\000\000\155\255\031\000\040\000\
\000\000\125\255\000\000\035\255"

let yygindex = "\000\000\
\000\000\000\000\249\255"

let yytablesize = 309
let yytable = "\013\000\
\014\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\041\000\042\000\043\000\044\000\045\000\046\000\047\000\048\000\
\049\000\050\000\003\000\004\000\005\000\006\000\001\000\007\000\
\015\000\023\000\024\000\016\000\008\000\027\000\028\000\007\000\
\033\000\007\000\007\000\052\000\002\000\013\000\000\000\013\000\
\013\000\013\000\013\000\009\000\013\000\013\000\013\000\013\000\
\000\000\000\000\013\000\013\000\000\000\000\000\013\000\000\000\
\013\000\013\000\015\000\000\000\015\000\015\000\015\000\015\000\
\000\000\015\000\015\000\015\000\015\000\000\000\000\000\015\000\
\015\000\000\000\000\000\015\000\000\000\015\000\015\000\014\000\
\000\000\014\000\014\000\014\000\014\000\000\000\014\000\014\000\
\014\000\014\000\000\000\000\000\014\000\014\000\000\000\000\000\
\014\000\000\000\014\000\014\000\016\000\000\000\016\000\016\000\
\016\000\016\000\000\000\016\000\016\000\016\000\016\000\000\000\
\000\000\016\000\016\000\000\000\000\000\016\000\000\000\016\000\
\016\000\024\000\000\000\024\000\024\000\024\000\024\000\000\000\
\024\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\000\000\024\000\024\000\010\000\000\000\
\010\000\010\000\010\000\010\000\000\000\010\000\010\000\021\000\
\000\000\021\000\021\000\021\000\021\000\000\000\021\000\021\000\
\000\000\010\000\010\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\000\021\000\032\000\000\000\017\000\018\000\
\000\000\019\000\020\000\021\000\022\000\023\000\024\000\025\000\
\026\000\027\000\028\000\029\000\000\000\030\000\031\000\051\000\
\017\000\018\000\000\000\019\000\020\000\021\000\022\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\000\000\030\000\
\031\000\017\000\018\000\000\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\000\000\
\030\000\031\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\000\000\030\000\031\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\008\000\
\000\000\008\000\008\000\008\000\008\000\000\000\008\000\008\000\
\009\000\000\000\009\000\009\000\009\000\009\000\000\000\009\000\
\009\000\011\000\000\000\011\000\011\000\011\000\011\000\000\000\
\011\000\011\000\012\000\000\000\012\000\012\000\012\000\012\000\
\000\000\012\000\012\000\022\000\000\000\022\000\022\000\022\000\
\022\000\000\000\022\000\022\000\023\000\000\000\023\000\023\000\
\023\000\023\000\000\000\023\000\023\000"

let yycheck = "\007\000\
\008\000\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\017\000\018\000\019\000\020\000\021\000\022\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\001\001\002\001\003\001\004\001\001\000\006\001\
\023\001\016\001\017\001\005\001\011\001\020\001\021\001\005\001\
\027\001\007\001\008\001\051\000\005\001\005\001\255\255\007\001\
\008\001\009\001\010\001\026\001\012\001\013\001\014\001\015\001\
\255\255\255\255\018\001\019\001\255\255\255\255\022\001\255\255\
\024\001\025\001\005\001\255\255\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\255\255\255\255\018\001\
\019\001\255\255\255\255\022\001\255\255\024\001\025\001\005\001\
\255\255\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\014\001\015\001\255\255\255\255\018\001\019\001\255\255\255\255\
\022\001\255\255\024\001\025\001\005\001\255\255\007\001\008\001\
\009\001\010\001\255\255\012\001\013\001\014\001\015\001\255\255\
\255\255\018\001\019\001\255\255\255\255\022\001\255\255\024\001\
\025\001\005\001\255\255\007\001\008\001\009\001\010\001\255\255\
\012\001\013\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\022\001\255\255\024\001\025\001\005\001\255\255\
\007\001\008\001\009\001\010\001\255\255\012\001\013\001\005\001\
\255\255\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\255\255\024\001\025\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\025\001\007\001\255\255\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\025\001\008\001\
\009\001\010\001\255\255\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\025\001\009\001\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\025\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\025\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\005\001\
\255\255\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\005\001\255\255\007\001\008\001\009\001\010\001\255\255\012\001\
\013\001\005\001\255\255\007\001\008\001\009\001\010\001\255\255\
\012\001\013\001\005\001\255\255\007\001\008\001\009\001\010\001\
\255\255\012\001\013\001\005\001\255\255\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\005\001\255\255\007\001\008\001\
\009\001\010\001\255\255\012\001\013\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  DBLSEMI\000\
  IF\000\
  THEN\000\
  ELSE\000\
  OR\000\
  AND\000\
  NOT\000\
  XOR\000\
  NAND\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  PLUSF\000\
  MINUSF\000\
  TIMESF\000\
  DIVIDEF\000\
  EQ\000\
  NEQ\000\
  LET\000\
  BE\000\
  IN\000\
  "

let yynames_block = "\
  FLOAT\000\
  INT\000\
  COMPOP\000\
  VARIABLE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'headEx) in
    Obj.repr(
# 33 "parser.mly"
                                 ( _1 )
# 240 "parser.ml"
               : Types.exprS))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                                 ( _1 )
# 247 "parser.ml"
               : 'headEx))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 41 "parser.mly"
                                 ( FloatS _1 )
# 254 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 42 "parser.mly"
                                 ( IntS _1 )
# 261 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "parser.mly"
                                 ( BoolS true )
# 267 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
                                 ( BoolS false )
# 273 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                                 ( IfS(_2,_4,_6) )
# 282 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                              ( OrS(_1 , _3) )
# 290 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                             ( AndS(_1 , _3) )
# 298 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                             ( NotS(_2) )
# 305 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                                 ( XOrS(_1,_3))
# 313 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                                 ( NAndS(_1,_3))
# 321 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                              ( ArithS("+",_1,_3))
# 329 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                                 ( ArithS("+.",_1,_3))
# 337 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                              ( ArithS("-",_1,_3))
# 345 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                                 ( ArithS("-.",_1,_3))
# 353 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 55 "parser.mly"
                               ( ArithS("*",_1,_3))
# 361 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 56 "parser.mly"
                                 ( ArithS("*.",_1,_3))
# 369 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 57 "parser.mly"
                                ( ArithS("/",_1,_3))
# 377 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 58 "parser.mly"
                                 ( ArithS("/.",_1,_3))
# 385 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                               ( CompS (_2, _1, _3) )
# 394 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                              ( EqS (_1, _3) )
# 402 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                             ( NeqS (_1, _3) )
# 410 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 62 "parser.mly"
                               ( LetS (_2, _4) )
# 418 "parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Types.exprS)
