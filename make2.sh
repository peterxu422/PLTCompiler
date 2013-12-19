#!/bin/bash
rm *.cmo;
rm *.cmi;
ocamllex scanner.mll;
ocamlyacc parser.mly; 
ocamlc -c ast.ml; 
ocamlc -c parser.mli; 
ocamlc -c scanner.ml; 
ocamlc -c parser.ml;
ocamlc -c helper.ml;
ocamlc -c -w -8 interpreter.ml;
ocamlc -o interpret helper.cmo ast.cmo parser.cmo scanner.cmo interpreter.cmo;
