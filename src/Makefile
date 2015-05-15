.PHONY: all doc FORCE clean 

all: extract.native split.native test_parse.native 

doc:
	ocamlbuild -use-ocamlfind p.docdir/index.html

clean:
	ocamlbuild -use-ocamlfind -clean
	rm -rf *.native split/ generated/ logs/

%.native: FORCE
	ocamlbuild -use-ocamlfind $@

FORCE:
