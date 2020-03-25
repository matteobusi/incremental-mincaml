TARGET=main

default: $(TARGET).byte

$(TARGET): default

native: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind -pkg batteries $@

%.byte:
	ocamlbuild -use-ocamlfind -pkg batteries $@

test: default
	ocamlbuild -use-ocamlfind -pkg batteries,ounit2 $@.native

texperiments:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,core,core_bench texperiments.native

mexperiments:
	ocamlbuild -use-ocamlfind -pkg batteries,landmarks mexperiments.native

all: native

clean:
	ocamlbuild -clean

.PHONY: all clean
