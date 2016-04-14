exception Desugar of string
exception Interp of string
exception Typecheck of string

type exprC = IntC of int 
                      | FloatC of float 
                      | BoolC of bool 
                      | IfC of (exprC * exprC * exprC) 
                      | ArithC of (string * exprC * exprC) 
                      | CompC of (string * exprC * exprC) 
                      | EqC of (exprC * exprC)

type exprS = IntS of int 
                      | FloatS of float 
                      | BoolS of bool 
                      | IfS of (exprS * exprS * exprS) 
                      | OrS of (exprS * exprS) 
                      | AndS of (exprS * exprS) 
                      | XOrS of (exprS * exprS)
                      | NAndS of (exprS * exprS)
                      | NotS of (exprS) 
                      | ArithS of string * exprS * exprS
                      | CompS of (string * exprS * exprS) 
                      | EqS of (exprS * exprS) 
                      | NeqS of (exprS * exprS)

type value = Int of int | Float of float | Bool of bool

type types = IntT
            | FloatT
            | BoolT

(* Environment lookup *)
type 'a env
val empty : 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : value env -> exprC -> value
val evaluate : exprC -> value

(* result post-processing *)
val valToString : value -> string
