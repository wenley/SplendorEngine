
COMPILER=ocamlopt
LIBRARY_INTERFACES=map.cmi
LIBRARY_DEPENDENCIES=str.cmxa
# map.cmx is already bundled into stdlib.cmxa

data: data.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

parse: globals.cmx data.cmx parse.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

read: globals.cmx data.cmx parse.cmx read.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

shuffle: shuffle.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

board: globals.cmx data.cmx parse.cmx read.cmx shuffle.cmx board.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

%.cmx: %.ml
	$(COMPILER) -c $<

%.cmi: %.mli
	$(COMPILER) -c $<

clean:
	rm -rf *.cmx *.cmi *.o
	rm -f splendor data parse read shuffle board
