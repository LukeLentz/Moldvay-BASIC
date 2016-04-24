# Moldvay-BASIC

To compile:

ocamlyacc parser.mly<br />
ocamllex lexer.mll<br />
ocamlc -c types.mli parser.mli lexer.ml parser.ml types.ml driver.ml<br />
ocamlc -o moldvay lexer.cmo parser.cmo types.cmo driver.cmo<br />
./moldvay

## Important Files
[details.md](https://github.com/LukeLentz/Moldvay-BASIC/blob/master/details.md)
[semantics.md](https://github.com/LukeLentz/Moldvay-BASIC/blob/master/semantics.md)
[examples.md](https://github.com/LukeLentz/Moldvay-BASIC/blob/master/examples.md)