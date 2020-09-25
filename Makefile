TARGET=main
ALL_LIBS=core,ppx_hash,ppx_compare,ppx_sexp_conv,ppx_fields_conv,core_bench,ocamlgraph,ounit2,landmarks,ppx_deriving.fold,ppx_deriving.show,ppx_deriving.map,ppx_deriving.iter

.PHONY: all clean

$(TARGET): default

default: $(TARGET).native

%.native:
	ocamlbuild  -tag thread -use-ocamlfind -pkg $(ALL_LIBS) $@

%.byte:
	ocamlbuild  -tag thread -use-ocamlfind -pkg $(ALL_LIBS) $@

unittest: default
	ocamlbuild  -tag thread -use-ocamlfind -pkg $(ALL_LIBS) $@.native

texperiments:
	ocamlbuild -tag thread -use-ocamlfind -pkg $(ALL_LIBS) texperiments.native

mexperiments:
	ocamlbuild  -tag thread -use-ocamlfind -pkg $(ALL_LIBS) mexperiments.native

dexperiments:
	ocamlbuild -tag thread -use-ocamlfind -pkg $(ALL_LIBS) dexperiments.native

wexperiments:
	ocamlbuild -tag thread -use-ocamlfind -pkg $(ALL_LIBS) wexperiments.native

experiments:
	ocamlbuild -tag thread -use-ocamlfind -pkg $(ALL_LIBS) experiments.native

all: default unittest mexperiments texperiments dexperiments wexperiments

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
