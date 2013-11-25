#!/bin/bash
rm *.cmo;
rm *.cmi;
dos2unix scanner.mll;
ocamllex scanner.mll;
echo hi
echo hihi
dos2unix parser.mly;
ocamlyacc parser.mly;
dos2unix ast.ml;
ocamlc -c ast.ml;
dos2unix parser.mli;
ocamlc -c parser.mli;
dos2unix scanner.ml;
ocamlc -c scanner.ml;
dos2unix parser.ml;
ocamlc -c parser.ml;
dos2unix interpreter.ml;
ocamlc -c interpreter.ml;
ocamlc -o interpret ast.cmo parser.cmo scanner.cmo interpreter.cmo;

# ocamllex scanner.mll
#	ocamlyacc parser.mly
#	ocamlc -c ast.ml
#	ocamlc -c parser.mli
#	ocamlc -c scanner.ml
#	ocamlc -c parser.ml
#	ocamlc -c interpreter.ml
#	ocamlc -o interpret ast.cmo parser.cmo scanner.cmo interpreter.cmo