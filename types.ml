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
                  | TupleC of (exprC * exprC)

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
                  | TupleS of (exprS * exprS)

type value = Int of int 
                  | Float of float
                  | Bool of bool

type types = IntT
                  | FloatT
                  | BoolT
                  | TupleT

type 'a env = (string * 'a) list

let empty = []

(* lookup : string -> 'a env -> 'a option *)
let rec lookup str env = match env with
  | []          -> None
  | (s,v) :: tl -> if s = str then Some v else lookup str tl
(* val bind :  string -> 'a -> 'a env -> 'a env *)
let bind str v env = (str, v) :: env

(*
   HELPER METHODS
   You may be asked to add methods here. You may also choose to add your own
   helper methods here.
*)
(* INTERPRETER *)

(* You will need to add cases here. *)
(* desugar : exprS -> exprC *)
let rec desugar exprS = match exprS with
  | IntS i        -> IntC i
  | FloatS f    -> FloatC f
  | BoolS b       -> BoolC b
  | IfS (a, b, c) -> IfC (desugar a, desugar b, desugar c)
  | AndS (x, y)   -> desugar (IfS (x, IfS (y, BoolS true, BoolS false), BoolS false))
  | NAndS (x, y) -> desugar (NotS (AndS (x, y)))
  | OrS (x, y)  -> desugar (IfS (x, BoolS true, (IfS (y, BoolS true, BoolS false))))
  | XOrS (x, y) -> desugar (IfS (x, IfS (y, BoolS false, BoolS true), IfS (y, BoolS true, BoolS false)))
  | NotS a       -> desugar (IfS (a, BoolS false, BoolS true))
  | ArithS (op, x, y)  -> ArithC (op, desugar x, desugar y)
  | CompS (op, x, y) -> CompC (op, desugar x, desugar y)
  | EqS (x, y) -> EqC (desugar x, desugar y)
  | NeqS (x, y) -> desugar (NotS (EqS (x, y)))
  | TupleS (x, y) -> TupleC (desugar x, desugar y)

  let arithEval op v1 v2 =
    match (op, v1, v2) with
    | ("+", Int v1, Int v2) -> Int (v1 + v2)
    | ("-", Int v1, Int v2) -> Int (v1 - v2)
    | ("*", Int v1, Int v2) -> Int (v1 * v2)
    | ("/", Int v1, Int v2) -> if (v2 = 0)
                                        then raise (Failure "Interp")
                                        else Int (v1 / v2)
    | ("+.", Float v1, Float v2) -> Float (v1 +. v2)
    | ("-.", Float v1, Float v2) -> Float (v1 -. v2)
    | ("*.", Float v1, Float v2) -> Float (v1 *. v2)
    | ("/.", Float v1, Float v2) -> if (v2 = 0.0)
                                                then raise (Failure "Interp")
                                                else Float (v1 /. v2)                                            
    | _ -> raise (Failure "Interp")                        

let compEval op v1 v2 =
    match (op, v1, v2) with
    | ("<", Int v1, Int v2) -> Bool (v1 < v2)
    | ("<=", Int v1, Int v2) -> Bool (v1 <= v2)
    | (">=", Int v1, Int v2) -> Bool (v1 >= v2)
    | (">", Int v1, Int v2) -> Bool (v1 > v2)
    | ("<", Float v1, Float v2) -> Bool (v1 < v2)
    | ("<=", Float v1, Float v2) -> Bool (v1 <= v2)
    | (">=", Float v1, Float v2) -> Bool (v1 >= v2)
    | (">", Float v1, Float v2) -> Bool (v1 > v2)
    | _ -> raise (Failure "Interp")

let eqEval v1 v2 =
    match (v1, v2) with
    | (Float x, Float y) -> Bool (x = y)
    | (Int x, Int y) -> Bool (x = y)
    | (Bool x, Bool y) -> Bool (x = y)
    | _ -> Bool false

 (*interp : Value env -> exprC -> value *)
let rec interp env r = match r with
  | IntC i         -> Int i
  | FloatC f       -> Float f
  | BoolC b        -> Bool b
  | ArithC (op, x, y) -> (match (x, y) with
                                    | (IntC v, IntC n) -> arithEval op (Int v) (Int n)
                                    | (FloatC v, FloatC n) -> arithEval op (Float v) (Float n)
                                    | (IntC v, ArithC n) -> arithEval op (Int v) (interp env (ArithC n))
                                    | (FloatC v, ArithC n) -> arithEval op (Float v) (interp env (ArithC n)) 

                                    | (ArithC v, IntC n) -> arithEval op (interp env (ArithC v)) (Int n)
                                    | (ArithC v, FloatC n) -> arithEval op (interp env (ArithC v)) (Float n)
                                    | (ArithC v, ArithC n) -> arithEval op (interp env (ArithC v)) (interp env (ArithC n))
                                    | _ -> raise (Failure "Interp") )
  | CompC (op, x, y) -> (match (x, y) with
                                      | (IntC x, IntC y) -> compEval op (Int x) (Int y)
                                      | (FloatC x, FloatC y) -> compEval op (Float x) (Float y)
                                      | _ -> raise (Failure "Interp"))
  | EqC (x, y) -> eqEval (interp env x) (interp env y)
  | IfC (test, op1, op2 ) -> 
     (match (interp env test) with 
     | Bool true -> interp env op1
     | Bool false -> interp env op2
     | _ -> raise (Failure "Not a Bool"))
  | TupleC (x, y) -> TupleC (interp env x, interp env y)

let rec tc env e =
    match e with
    |IntC i -> IntT
    |FloatC c -> FloatT
    |BoolC b -> BoolT
    (*Need ArithC, IfC, CompC, EqC *)
    | TupleC t -> TupleT
    | _ -> raise (Failure "Typecheck")

(* evaluate : exprC -> val *)
(*let evaluate exprC = exprC |> interp []*)
let evaluate exprC = 
  (*return pair of type and result*)
  let ts = tc [] exprC 
    in let vs = interp [] exprC
      in (ts,vs)


(* You will need to add cases to this function as you add new value types. *)
let rec valToString r = match r with
  | Float i           -> string_of_float i
  | Int i           -> string_of_int i
  | Bool b          -> string_of_bool b


let typeToString t = 
    match t with
    | IntT -> "IntT "
    | FloatT -> "FloatT "
    | BoolT -> "BoolT "
    | TupleT (x, y) -> (typeToString (tc x) * typeToString (tc y))

let pairToString (t,r) = 
    (typeToString t) ^ (valToString r)
