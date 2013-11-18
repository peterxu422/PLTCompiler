#!/bin/bash
rm *.cmo;
rm *.cmi;
ocamllex scanner.mll;
ocamlyacc parser.mly; 
ocamlc -c ast.ml; 
ocamlc -c parser.mli; 
ocamlc -c scanner.ml; 
ocamlc -c parser.ml;
ocamlc -c interpreter.ml;
ocamlc -o interpret ast.cmo parser.cmo scanner.cmo interpreter.cmo;