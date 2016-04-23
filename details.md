added logical expressions like and, xor, and or to the surface language since they can all convert to an if statement

getting final typecheck error when parsing functions ie. fun xs : type -> expr is not recognized as a FunC, it goes to the final match case which raises an exception
but functions that simply return a value like fun xs : int -> 4 are fine