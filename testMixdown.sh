#!/bin/bash

#rm interpret
#rm *.cmo;
#rm *.cmi;
#ocamllex scanner.mll;
#ocamlyacc parser.mly; 
#ocamlc -c ast.ml; 
#ocamlc -c parser.mli; 
#ocamlc -c scanner.ml; 
#ocamlc -c parser.ml;
#ocamlc -c interpreter.ml;
#ocamlc -o interpret ast.cmo parser.cmo scanner.cmo interpreter.cmo;
./interpret < $1
javac -classpath ./jfugue-4.0.3.jar BytecodeTranslator.java
if [ ! -z $2 ] 
then 
	java -cp jfugue-4.0.3.jar:. BytecodeTranslator bytecode $2
    # $2 was given
else
	java -cp jfugue-4.0.3.jar:. BytecodeTranslator bytecode llb-write.mid
    # $2 was not given
fi