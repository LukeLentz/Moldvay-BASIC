exception Desugar of string
exception Interp of string
exception Typecheck of string

type types = IntT
                  | FloatT
                  | BoolT
                  | TupleT of types list
                  | ListT of types list
                  | AnyT
                  | VarT
                  
type exprC = IntC of int 
                  | FloatC of float 
                  | BoolC of bool 
                  | IfC of (exprC * exprC * exprC) 
                  | ArithC of (string * exprC * exprC) 
                  | CompC of (string * exprC * exprC) 
                  | EqC of (exprC * exprC)
                  | TupleC of exprC list
                  | ListC of exprC list
                  | HeadC of exprC
                  | TailC of exprC
                  | ConsC of (exprC * exprC)
                  | VarC of string
                  | LetC of (string * exprC * exprC)
                  | FunC of (string * types * exprC)
                  | ArgC of string
                  | CallC of exprC

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
                  | TupleS of exprS list
                  | ListS of exprS list
                  | HeadS of exprS
                  | TailS of exprS
                  | ConsS of (exprS * exprS)
                  | VarS of string
                  | LetS of (string * exprS * exprS)
                  | FunS of (string * types * exprS)
                  | ArgS of string
                  | CallS of exprS

type value = Int of int 
                  | Float of float
                  | Bool of bool
                  | Tuple of value list
                  | List of value list
                  | Var of string
                  | Any





(* Environment lookup *)
type 'a env
val empty : 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : value env -> exprC -> value 
val evaluate : exprC -> types * value

(* result post-processing *)
val valToString : value -> string
val pairToString : types * value -> string
