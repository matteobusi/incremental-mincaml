TARGET=main

default: $(TARGET).byte

$(TARGET): default

native: $(TARGET).native

%.native:
	ocamlbuild -tag thread -use-ocamlfind -pkg ounit2,batteries $@

%.byte:
	ocamlbuild -tag thread -use-ocamlfind -pkg ounit2,batteries $@

test: default
	ocamlbuild -tag thread -use-ocamlfind -pkg ounit2,batteries,benchmark $@.byte

tbenchmark: default
	ocamlbuild -tag thread -use-ocamlfind -pkg ounit2,batteries,benchmark $@.native

tmemory: default
	ocamlbuild -tag thread -use-ocamlfind -pkg ounit2,batteries,landmarks,landmarks.ppx $@.native

test_prof: default
	ocamlbuild ocamlopt -tag -p thread -use-ocamlfind -pkg ounit2,batteries,benchmark test.native

all: native test tbenchmark tmemory

clean:
	ocamlbuild -clean

.PHONY: all clean
