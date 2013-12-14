OBJS = helper.cmo ast.cmo parser.cmo scanner.cmo interpreter.cmo

all: interpret

interpret: $(OBJS)
	ocamlc -o interpret $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll
        
parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<
        
clean:
	rm -rf *.cmo
	rm -rf *.cmi