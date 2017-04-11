
COMPILER=ocamlopt

splendor : data.cmx display.cmx validation.cmx noble.cmx action.cmx prompt.cmx engine.cmx 
	$(COMPILER) -o $@ $^

%.cmx: %.ml
	$(COMPILER) -c $<

clean:
	rm -f *.cmx *.o *.cmi
	rm -f splendor
