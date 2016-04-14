() denotes optional arguments
* denotes indefinite amount

// for in-line comments

/* 
this is used
for mulit-line
comments
*/

xor : bool -> bool -> bool
    returns true if only one of the given expressions is true, otherwise returns false

and : bool -> bool -> bool
    returns true if both given expressions are true

or : bool -> bool -> bool
    returns true if at least one of the given expressions are true

not : bool -> bool
    returns false if a is true, vice versa

if e1 then e2 else e3
	returns e2 if e1 is true, else it returns e3

def string e1 (e2)*
	binds e1 to the string, equivalent to let statement
	(e2)* are additional expressions equivalent to let in

Addition:
    arith + int int
    arith +. float float

Subtraction:
    arith - int int
    arith -. float float

Multiplication:
    arith * int int
    arith *. float float

Division:
    arith / int int
    ariht /. float float
        throw exception if division by 0

Tuples:
    to access part of a tuple
        t.0 (1, 2, 3) returns 1
        t.1 returns 2, etc
        


