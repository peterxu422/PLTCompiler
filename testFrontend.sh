#!/bin/bash

rm printAst
rm *.cmo;
rm *.cmi;
ocamllex scanner.mll;
ocamlyacc parser.mly; 
ocamlc -c ast.ml; 
ocamlc -c parser.mli; 
ocamlc -c scanner.ml; 
ocamlc -c parser.ml;
ocamlc -c printast.ml;
ocamlc -o printAst ast.cmo parser.cmo scanner.cmo printast.cmo;
./printAst < tests/frontEndTest.llb
