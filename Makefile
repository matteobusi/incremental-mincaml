TARGET=main

default: $(TARGET).byte

$(TARGET): default

native: $(TARGET).native

#test: test.native #mv $@ $*

%.native:
	ocamlbuild -use-ocamlfind $@

%.byte: clean
	ocamlbuild -use-ocamlfind $@

test: default
	ocamlbuild -use-ocamlfind $@.byte

all: default native

clean:
	ocamlbuild -clean

.PHONY: all clean
