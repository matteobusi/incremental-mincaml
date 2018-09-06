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

all: native test tbenchmark

clean:
	ocamlbuild -clean

.PHONY: all clean