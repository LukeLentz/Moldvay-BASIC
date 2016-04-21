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
                  | TupleC of exprC list
                  | ListC of exprC list
                  | VarC of string
                  | LetC of (string * exprC)

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
                  | VarS of string
                  | LetS of (string * exprS)

type value = Int of int 
                  | Float of float
                  | Bool of bool
                  | Tuple of value list
                  | List of value list
                  | Env of (string * exprC) list
                  | Var of string

type types = IntT
                  | FloatT
                  | BoolT
                  | TupleT of types list
                  | ListT of types list
                  | AnyT
                  | VarT


(* Environment lookup *)
type 'a env
val empty : 'a env
val lookup : string -> 'a env -> 'a option
val bind :  string -> 'a -> 'a env -> 'a env

(* Interpreter steps *)
val desugar : exprS -> exprC
val interp : (string * exprC) list -> exprC -> value
val evaluate : exprC -> types * value

(* result post-processing *)
val valToString : value -> string
val pairToString : types * value -> string
