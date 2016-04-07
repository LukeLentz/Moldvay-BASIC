%{
  open Types
%}

%token <float> FLOAT
%token <int> INT
%token TRUE FALSE
%token DBLSEMI
%token IF THEN ELSE
%token OR AND NOT XOR NAND
%token PLUS MINUS TIMES DIVIDE
%token <string> COMPOP 
%token EQ NEQ
%nonassoc FLOAT
%nonassoc ELSE
%left OR AND XOR NAND
%nonassoc EQ NEQ
%nonassoc NOT
%nonassoc COMPOP
%left PLUS MINUS
%left TIMES DIVIDE 
%token DBLSEMI


%start main
%type <Types.exprS> main
%%

main:
  | headEx DBLSEMI               { $1 }
;

headEx:
  | expr                         { $1 }
;

expr:
  | FLOAT                        { FloatS $1 }
  | INT                          { IntS $1 }
  | TRUE                         { BoolS true }
  | FALSE                        { BoolS false }
  | IF expr THEN expr ELSE expr  { IfS($2,$4,$6) }
  | expr OR expr 				         { OrS($1 , $3) }
  | expr AND expr 				       { AndS($1 , $3) }
  | NOT expr 					           { NotS($2) }
  | expr XOR expr                { XOrS($1,$3)}
  | expr NAND expr               { NAndS($1,$3)}
  | expr PLUS expr 				       { ArithS("+",$1,$3)}
  | expr MINUS expr 			       { ArithS("-",$1,$3)}
  | expr TIMES expr 		         { ArithS("*",$1,$3)}
  | expr DIVIDE expr 		 	       { ArithS("/",$1,$3)}
  | expr COMPOP expr 			       { CompS ($2, $1, $3) }
  | expr EQ expr 				         { EqS ($1, $3) }
  | expr NEQ expr 				       { NeqS ($1, $3) }
;