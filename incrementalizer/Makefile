TARGET=main

default: $(TARGET).byte

$(TARGET): default

native: $(TARGET).native

%.native:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries $@

%.byte:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries $@

# test: default
# 	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,benchmark $@.byte

all: native

clean:
	ocamlbuild -clean

.PHONY: all clean
