let rec modn a n =
    // int -> int -> int
    if < a n // if n is greater, a is the remainder
    then a
    else if / a n == 0 // no remainder
            then 0
            else modn - a n n // keep recurring until < a n
