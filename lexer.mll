{
  open Parser
  exception Eof
  exception Unrecognized
}

let any = _
let digit = ['0'-'9']
let sign = ['+' '-']
let frac = '.' digit+
let exp = ['e' 'E'] sign? digit+
let white = [' ' '\t' '\n' '\r']+ | "//" ([^ '\n' '\r'])*
let newline = '\n' | '\r' | "\r\n"
let dblsemi = ";;"
let float = (digit+ '.' | digit* frac) exp?
let int = (digit)+
let true = "true" | "#t"
let false = "false" | "#f"
let comp = ">" | ">=" | "<" | "<="
let var = ['a'-'z' 'A'-'Z']+
let arg = "arg"

rule token = parse
  | white       { token lexbuf }
  | newline     { token lexbuf }
  | dblsemi     { DBLSEMI }
  | float as x  { FLOAT (float_of_string x) }
  | int as x    { INT (int_of_string x) }
  | true        { TRUE }
  | false       { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "or"        { OR }
  | "and"       { AND }
  | "not"       { NOT }
  | "xor"     { XOR }
  | "nand"    { NAND }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { TIMES }
  | "/"         { DIVIDE }
  | "+."     { PLUSF }
  | "-."     { MINUSF }
  | "*."     { TIMESF }
  | "/."     { DIVIDEF }
  | "=="        { EQ }
  | "!="        { NEQ }
  | "let"  { LET }
  | "="  { BE }
  | "in"  { IN }
  | "("    { OPEN }
  | ")"     { CLOSE }
  | "fun"  { FUN }
  | ":"      { OF }
  | "->"   { TO }
  | "int"    { INTTYPE }
  | "float" { FLOATTYPE }
  | "bool"  { BOOLTYPE }
  | "tuple"  { TUPLETYPE }
  | "list"         { LISTTYPE }
  | "["             { OPENLST }
  | "]"             { CLOSELST }
  | "::"          { CONS }
  | "["             { OPENLST }
  | "]"             { CLOSELST }
  | arg as x  { ARG x }
  | var as x { VARIABLE x}
  | comp as s   { COMPOP s }
  | eof         { raise Eof }
  | any         { raise Unrecognized }