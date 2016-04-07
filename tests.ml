open Types

let t0a = evaluate (IntC 2) = Int 2
let t0a = evaluate (FloatC 2.3) = Float 2.3
let t0c = desugar (IntS 2) = IntC 2
let t0c = desugar (FloatS 2.3) = FloatC 2.3

let t1a = evaluate (BoolC true) = Bool true
let t1b = evaluate (BoolC false) = Bool false
let t1c = desugar (BoolS true) = BoolC true
let t1d = desugar (BoolS false) = BoolC false

let t2a = evaluate (IfC (BoolC true, FloatC 2.3, FloatC 4.3)) = Float 2.3
let t2b = evaluate (IfC (IfC (BoolC false, BoolC false, BoolC true), IntC 2, IntC 4)) = Int 2
let t2c = desugar (IfS (BoolS true, FloatS 2.3, FloatS 4.3)) = IfC(BoolC true, FloatC 2.3, FloatC 4.3)
let t2d = evaluate (desugar (NotS (BoolS true))) = Bool false
let t2e = evaluate (desugar (NotS (BoolS false))) = Bool true
let t2f = evaluate (desugar (OrS (BoolS true, BoolS true))) = Bool true
let t2g = evaluate (desugar (OrS (BoolS false, BoolS true))) = Bool true
let t2h = evaluate (desugar (OrS (BoolS false, BoolS false))) = Bool false
let t2i = evaluate (desugar (AndS (BoolS false, BoolS true))) = Bool false
let t2j = evaluate (desugar (AndS (BoolS true, BoolS false))) = Bool false
let t2k = evaluate (desugar (AndS (BoolS true, BoolS true))) = Bool true
let t2l = evaluate (desugar (XOrS (BoolS true, BoolS false))) = Bool true
let t2l = evaluate (desugar (XOrS (BoolS true, BoolS true))) = Bool false
let t2m = evaluate (desugar (NAndS (BoolS true, BoolS true))) = Bool false