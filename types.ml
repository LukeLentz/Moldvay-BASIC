type exprC = IntC of int | FloatC of float | BoolC of bool | IfC of (exprC * exprC * exprC) | ArithC of (exprC * exprC * exprC)

type exprS = IntS of int | FloatS of float | BoolS of bool | IfS of (exprS * exprS * exprS) | OrS of (exprS * exprS) | AndS of (exprS * exprS) | XOrS of (exprS * exprS) | NotS of (exprS) | ArithS of (exprS   * exprS * exprS)

type value = Int of int | Float of float | Bool of bool