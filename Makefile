TARGET=main

default: $(TARGET).byte

$(TARGET): default

native: $(TARGET).native

#test: test.native #mv $@ $*

%.native:
	ocamlbuild -use-ocamlfind $@

%.byte: clean 
	ocamlbuild -use-ocamlfind $@

cachetest: default
	ocamlbuild -use-ocamlfind cachetest.ml

all: default native

clean:
	ocamlbuild -clean

.PHONY: all clean
