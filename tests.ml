open Types

let t0a = evaluate (IntC 2) = Int 2
let t0a = evaluate (FloatC 2.3) = Float 2.3
let t0c = desugar (IntS 2) = IntC 2.3
let t0c = desugar (FloatS 2.3) = FloatC 2.3