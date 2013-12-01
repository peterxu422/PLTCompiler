default: ast parser scanner interpreter
	ocamlc -o interpret ast.cmo parser.cmo scanner.cmo interpreter.cmo

ast: ast.ml
	ocamlc -c ast.ml

parser: parser.mli
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml

scanner: scanner.ml
	ocamllex scanner.mll
	ocamlc -c scanner.ml

interpreter: interpreter.ml
	ocamlc -c interpreter.ml

test:
	./interpret < ./tests/nathan.tests.llb

.PHONY:
clean:
	rm -rf *.cmo
	rm -rf *.cmi
