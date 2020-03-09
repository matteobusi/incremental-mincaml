TARGET=main

default: $(TARGET).byte

$(TARGET): default

native: $(TARGET).native

%.native:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries $@

%.byte:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries $@

test: default
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,benchmark,ounit2 $@.native

bench:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,benchmark,ounit2 tbenchmark.native

all: native

clean:
	ocamlbuild -clean

.PHONY: all clean
