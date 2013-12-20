all:
	javac -classpath ./jfugue-4.0.3.jar BytecodeTranslator.java
	ocamllex scanner.mll;
	ocamlyacc parser.mly; 
	ocamlc -i ast.ml > ast.mli;
	ocamlc -c ast.mli;
	ocamlc -c ast.ml; 
	ocamlc -c parser.mli; 
	ocamlc -c scanner.ml; 
	ocamlc -c parser.ml;
	ocamlc -c helper.ml;
	ocamlc -c interpreter.ml;
	ocamlc -o interpret helper.cmo ast.cmo parser.cmo scanner.cmo interpreter.cmo;

        
clean:
	rm -rf *.cmo
	rm -rf *.cmi
	rm -rf *.mli
	rm BytecodeTranslator.class
	rm interpret
