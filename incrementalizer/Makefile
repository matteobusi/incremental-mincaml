TARGET=main

default: $(TARGET).byte

$(TARGET): default

native: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind -pkg batteries $@

%.byte:
	ocamlbuild -use-ocamlfind -pkg batteries $@

test: default
	ocamlbuild -use-ocamlfind -pkg batteries,benchmark,ounit2 $@.native

tbench: clean
	ocamlbuild -use-ocamlfind -pkg batteries,benchmark tbenchmark.native

experiments: clean
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,core,core_bench experiments.native

mbench:
	ocamlbuild -use-ocamlfind -pkg batteries,landmarks mbenchmark.native

all: native

clean:
	ocamlbuild -clean

.PHONY: all clean
