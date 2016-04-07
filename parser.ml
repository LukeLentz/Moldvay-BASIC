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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Types
# 28 "parser.ml"
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
  275 (* EQ *);
  276 (* NEQ *);
    0|]

let yytransl_block = [|
  257 (* FLOAT *);
  258 (* INT *);
  274 (* COMPOP *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\001\000\006\000\003\000\
\003\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\003\000\004\000\005\000\006\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\015\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yydgoto = "\002\000\
\009\000\010\000\011\000"

let yysindex = "\006\000\
\122\255\000\000\000\000\000\000\000\000\000\000\122\255\122\255\
\000\000\016\255\102\255\077\255\244\254\000\000\122\255\122\255\
\122\255\122\255\122\255\122\255\122\255\122\255\122\255\122\255\
\122\255\122\255\173\255\173\255\173\255\173\255\113\255\113\255\
\000\000\000\000\048\255\244\254\244\254\090\255\122\255\102\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\255\000\000\047\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\127\255\136\255\145\255\154\255\015\255\031\255\
\000\000\000\000\063\255\163\255\172\255\000\000\000\000\072\255"

let yygindex = "\000\000\
\000\000\000\000\249\255"

let yytablesize = 193
let yytable = "\012\000\
\013\000\019\000\020\000\021\000\022\000\023\000\001\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\013\000\014\000\013\000\013\000\013\000\
\013\000\002\000\013\000\013\000\013\000\013\000\000\000\040\000\
\013\000\013\000\013\000\014\000\000\000\014\000\014\000\014\000\
\014\000\000\000\014\000\014\000\014\000\014\000\000\000\000\000\
\014\000\014\000\014\000\010\000\000\000\010\000\010\000\010\000\
\010\000\000\000\010\000\010\000\000\000\019\000\020\000\021\000\
\022\000\010\000\010\000\017\000\000\000\017\000\017\000\017\000\
\017\000\000\000\017\000\017\000\007\000\000\000\007\000\007\000\
\000\000\017\000\017\000\026\000\000\000\015\000\016\000\000\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\039\000\015\000\016\000\000\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\015\000\016\000\
\000\000\017\000\018\000\019\000\020\000\021\000\022\000\023\000\
\024\000\025\000\003\000\004\000\005\000\006\000\000\000\007\000\
\021\000\022\000\000\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000\008\000\008\000\009\000\000\000\009\000\009\000\
\009\000\009\000\000\000\009\000\009\000\011\000\000\000\011\000\
\011\000\011\000\011\000\000\000\011\000\011\000\012\000\000\000\
\012\000\012\000\012\000\012\000\000\000\012\000\012\000\018\000\
\000\000\018\000\018\000\018\000\018\000\000\000\018\000\018\000\
\019\000\000\000\019\000\019\000\019\000\019\000\000\000\019\000\
\019\000\000\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000"

let yycheck = "\007\000\
\008\000\014\001\015\001\016\001\017\001\018\001\001\000\015\000\
\016\000\017\000\018\000\019\000\020\000\021\000\022\000\023\000\
\024\000\025\000\026\000\005\001\005\001\007\001\008\001\009\001\
\010\001\005\001\012\001\013\001\014\001\015\001\255\255\039\000\
\018\001\019\001\020\001\005\001\255\255\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\255\255\255\255\
\018\001\019\001\020\001\005\001\255\255\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\255\255\014\001\015\001\016\001\
\017\001\019\001\020\001\005\001\255\255\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\005\001\255\255\007\001\008\001\
\255\255\019\001\020\001\007\001\255\255\009\001\010\001\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\008\001\009\001\010\001\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\009\001\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\001\001\002\001\003\001\004\001\255\255\006\001\
\016\001\017\001\255\255\005\001\011\001\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\005\001\255\255\007\001\008\001\
\009\001\010\001\255\255\012\001\013\001\005\001\255\255\007\001\
\008\001\009\001\010\001\255\255\012\001\013\001\005\001\255\255\
\007\001\008\001\009\001\010\001\255\255\012\001\013\001\005\001\
\255\255\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\005\001\255\255\007\001\008\001\009\001\010\001\255\255\012\001\
\013\001\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001"

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
  EQ\000\
  NEQ\000\
  "

let yynames_block = "\
  FLOAT\000\
  INT\000\
  COMPOP\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'headEx) in
    Obj.repr(
# 30 "parser.mly"
                                 ( _1 )
# 180 "parser.ml"
               : Types.exprS))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                                 ( _1 )
# 187 "parser.ml"
               : 'headEx))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 38 "parser.mly"
                                 ( FloatS _1 )
# 194 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 39 "parser.mly"
                                 ( IntS _1 )
# 201 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                                 ( BoolS true )
# 207 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                                 ( BoolS false )
# 213 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                                 ( IfS(_2,_4,_6) )
# 222 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                              ( OrS(_1 , _3) )
# 230 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 44 "parser.mly"
                             ( AndS(_1 , _3) )
# 238 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                             ( NotS(_2) )
# 245 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                                 ( XOrS(_1,_3))
# 253 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                                 ( NAndS(_1,_3))
# 261 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                              ( ArithS("+",_1,_3))
# 269 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                              ( ArithS("-",_1,_3))
# 277 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                               ( ArithS("*",_1,_3))
# 285 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                                ( ArithS("/",_1,_3))
# 293 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 52 "parser.mly"
                               ( CompS (_2, _1, _3) )
# 302 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                              ( EqS (_1, _3) )
# 310 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 54 "parser.mly"
                             ( NeqS (_1, _3) )
# 318 "parser.ml"
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
