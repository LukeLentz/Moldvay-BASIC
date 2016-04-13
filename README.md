# Moldvay-BASIC

To compile:

ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c types.mli parser.mli lexer.ml parser.ml types.ml driver.ml
ocamlc -o moldvay lexer.cmo parser.cmo types.cmo driver.cmo
./moldvay