test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

zip:
	zip 3dtictactoesrc.zip *.ml* *.json

zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f checktypes.ml
	rm -f 3dtictactoesrc.zip
