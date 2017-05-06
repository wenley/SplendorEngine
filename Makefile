
COMPILER=ocamlopt
LIBRARY_DEPENDENCIES=str.cmxa

splendor : data.cmx display.cmx validation.cmx noble.cmx action.cmx prompt.cmx engine.cmx 
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

data: data.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

display: display.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

validation: data.cmx validation.cmx 
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

noble: data.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

action: data.cmx validation.cmx action.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

prompt: data.cmx validation.cmx action.cmx prompt.cmx
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

engine: data.cmx display.cmx validation.cmx noble.cmx action.cmx prompt.cmx engine.cmx 
	$(COMPILER) -o $@ $(LIBRARY_DEPENDENCIES) $^

%.cmx: %.ml
	$(COMPILER) -c $<

clean:
	rm -f *.cmx *.o *.cmi
	rm -f splendor
