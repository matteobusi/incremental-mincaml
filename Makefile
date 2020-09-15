TARGET=main

.PHONY: all clean

$(TARGET): default

default: $(TARGET).native

%.native:
	ocamlbuild -use-ocamlfind -pkg batteries,ocamlgraph $@

%.byte:
	ocamlbuild -use-ocamlfind -pkg batteries,ocamlgraph $@

test: default
	ocamlbuild -use-ocamlfind -pkg batteries,ounit2,ocamlgraph $@.native

texperiments:
	ocamlbuild -tag thread -use-ocamlfind -pkg batteries,core,ocamlgraph,core_bench texperiments.native

mexperiments:
	ocamlbuild -use-ocamlfind -pkg batteries,landmarks,ocamlgraph mexperiments.native

dexperiments:
	ocamlbuild -tag thread -use-ocamlfind -cflag -unsafe -pkg batteries,core,core_bench,qcheck dexperiments.native

all: default test mexperiments texperiments

clean:
	ocamlbuild -clean

min-rt:
	# Print the explicitly typed program
	../min-caml/min-caml -tprint src/fun/examples/min-rt/min-rt;
	# Add the runtime needed by the ocaml compiler
	cat src/fun/examples/min-rt/miniMLRuntime.ml src/fun/examples/min-rt/min-rt.t.ml > src/fun/examples/min-rt/minrt.o.ml
	# Compile the program, as a test and render a sample model
	ocamlc -o src/fun/examples/min-rt/minrt src/fun/examples/min-rt/minrt.o.ml;
	src/fun/examples/min-rt/minrt < src/fun/examples/min-rt/models/shuttle.sld > res.ppm;
	display res.ppm;
	rm res.ppm;
	# And finally, typecheck the program w/o changes
	./main.native src/fun/examples/min-rt/min-rt.t.ml src/fun/examples/min-rt/min-rt.t.ml
