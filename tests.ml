open Types

let t0a = evaluate (IntC 2) = (IntT, Int 2)
let t0a = evaluate (FloatC 2.3) = (FloatT, Float 2.3)
let t0c = desugar (IntS 2) = IntC 2
let t0c = desugar (FloatS 2.3) = FloatC 2.3

let t1a = evaluate (BoolC true) = (BoolT, Bool true)
let t1b = evaluate (BoolC false) = (BoolT, Bool false)
let t1c = desugar (BoolS true) = (BoolC true)
let t1d = desugar (BoolS false) = (BoolC false)

let t2a = evaluate (IfC (BoolC true, FloatC 2.3, FloatC 4.3)) = (FloatT, Float 2.3)
let t2b = evaluate (IfC (IfC (BoolC false, BoolC false, BoolC true), IntC 2, IntC 4)) = (IntT, Int 2)
let t2c = desugar (IfS (BoolS true, FloatS 2.3, FloatS 4.3)) = IfC(BoolC true, FloatC 2.3, FloatC 4.3)
let t2d = evaluate (desugar (NotS (BoolS true))) = (BoolT, Bool false)
let t2e = evaluate (desugar (NotS (BoolS false))) = (BoolT, Bool true)
let t2f = evaluate (desugar (OrS (BoolS true, BoolS true))) = (BoolT, Bool true)
let t2g = evaluate (desugar (OrS (BoolS false, BoolS true))) = (BoolT, Bool true)
let t2h = evaluate (desugar (OrS (BoolS false, BoolS false))) = (BoolT, Bool false)
let t2i = evaluate (desugar (AndS (BoolS false, BoolS true))) = (BoolT, Bool false)
let t2j = evaluate (desugar (AndS (BoolS true, BoolS false))) = (BoolT, Bool false)
let t2k = evaluate (desugar (AndS (BoolS true, BoolS true))) = (BoolT, Bool true)
let t2l = evaluate (desugar (XOrS (BoolS true, BoolS false))) = (BoolT, Bool true)
let t2l = evaluate (desugar (XOrS (BoolS true, BoolS true))) = (BoolT, Bool false)
let t2m = evaluate (desugar (NAndS (BoolS true, BoolS true))) = (BoolT, Bool false)

let t3a = evaluate (ArithC ("+", IntC 1, IntC 1)) = (IntT, Int 2)
let t3b = evaluate (ArithC ("-." , FloatC 2.5 , FloatC 1.)) = (FloatT, Float 1.5)
let t3c = evaluate (ArithC ("*." , FloatC 5. , FloatC 5.)) = (FloatT, Float 25.)
let t3d = evaluate (ArithC ("/" , IntC 10 , IntC 5)) = (IntT, Int 2)
let t3e = desugar (ArithS ("+", IntS 1, IntS 1)) = ArithC("+", IntC 1, IntC 1)

let t4a = evaluate (CompC (">", IntC 20, IntC 1)) = (BoolT, Bool true) 
let t4b = evaluate (CompC (">", FloatC 1., FloatC 20.)) = (BoolT, Bool false)
let t4c = evaluate (CompC (">=", FloatC 20., FloatC 20.)) = (BoolT, Bool true) 
let t4d = evaluate (CompC (">=", IntC 0, IntC 1)) = (BoolT, Bool false) 
let t4e = evaluate (CompC ("<", IntC 1, IntC 1)) = (BoolT, Bool false) 
let t4f = evaluate (CompC ("<", FloatC 0., FloatC 1.)) = (BoolT, Bool true) 
let t4g = evaluate (CompC ("<=", FloatC 20., FloatC 100.)) = (BoolT, Bool true) 
let t4h = evaluate (CompC ("<=", IntC 20, IntC 1)) = (BoolT, Bool false)
let t4i = desugar (CompS (">", FloatS 1., FloatS 1.)) = CompC(">", FloatC 1., FloatC 1.)

let t5a = evaluate (EqC (IntC 20, IntC 20)) = (BoolT, Bool true) 
let t5b = evaluate (EqC (FloatC 220., FloatC 20.)) = (BoolT, Bool false)
let t5c = evaluate (EqC (BoolC false, BoolC false)) = (BoolT, Bool true) 
let t5d = evaluate (EqC (BoolC true, BoolC true)) = (BoolT, Bool true)

let t5e = try ((evaluate (EqC (FloatC 20., BoolC true))); false) with
                | Failure "Typecheck" -> true
                | _ -> false
let t5f = desugar  (EqS (FloatS 20., BoolS true)) = EqC (FloatC 20., BoolC true)
let t5g = desugar  (NeqS (FloatS 20., BoolS true)) =  IfC (EqC (FloatC 20., BoolC true), BoolC false, BoolC true) 


let t6a = desugar (TupleS [FloatS 2. ; IntS 5]) = (TupleC [FloatC 2. ; IntC 5])
let t6b = evaluate (desugar (TupleS [FloatS 1.;BoolS true ; IntS 4])) = (TupleT [FloatT; BoolT; IntT], Tuple [Float 1.; Bool true; Int 4])    
let t6c = desugar (ListS [FloatS 2. ; FloatS 5.]) = (ListC [FloatC 2. ; FloatC 5.])
let t6d = try (evaluate(desugar (ListS [FloatS 2. ; IntS 5])); false) with
                | Failure "Typecheck" -> true
                | _ -> false



let t7a = desugar (VarS "Hello") = VarC "Hello"