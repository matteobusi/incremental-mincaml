TARGET=main

default: $(TARGET).byte

$(TARGET): default

native: $(TARGET).native

%.native:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries $@

%.byte:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries $@

test: default
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,benchmark $@.byte

tbenchmark: default 
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,benchmark $@.native

tmemory: default 
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,landmarks,landmarks.ppx $@.native

test_prof: default
	ocamlbuild ocamlopt -tag -p thread -use-ocamlfind -pkg batteries,benchmark test.native

all: native test tbenchmark tmemory

clean:
	ocamlbuild -clean

.PHONY: all clean
