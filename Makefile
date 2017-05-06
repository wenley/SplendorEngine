
COMPILER=ocamlopt
LIBRARY_INTERFACES=map.cmi
LIBRARY_DEPENDENCIES=str.cmxa
# map.cmx is already bundled into stdlib.cmxa

data: data.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

parse: data.cmx parse.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

%.cmx: %.ml
	$(COMPILER) -c $<

%.cmi: %.mli
	$(COMPILER) -c $<

clean:
	rm -rf *.cmx *.cmi
	rm -f splendor data data_string
