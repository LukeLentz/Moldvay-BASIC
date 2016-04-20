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
let variable = "x"

rule token = parse
  | white       { token lexbuf }
  | newline     { token lexbuf }
  | dblsemi     { DBLSEMI }
  | float as x  { FLOAT (float_of_string x) }
  | int as x    { INT (int_of_string x) }
  | true        { TRUE }
  | false       { FALSE }
  | variable as s { VARIABLE s}
  | "if" 		{ IF }
  | "then"		{ THEN }
  | "else"		{ ELSE }
  | "or"		{ OR }
  | "and" 		{ AND }
  | "not" 		{ NOT }
  | "xor"     { XOR }
  | "nand"    { NAND }
  | "+"			{ PLUS }
  | "-"			{ MINUS }
  | "*" 		{ TIMES }
  | "/" 		{ DIVIDE }
  | "+."     { PLUSF }
  | "-."     { MINUSF }
  | "*."     { TIMESF }
  | "/."     { DIVIDEF }
  | "==" 		{ EQ }
  | "!="        { NEQ }
  | comp as s   { COMPOP s }
  | "define" { DEFINE }
  | eof         { raise Eof }
  | any         { raise Unrecognized }