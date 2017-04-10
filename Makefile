
COMPILER=ocamlopt

splendor : data.cmx engine.cmx 
	$(COMPILER) -o $@ $^

%.cmx: %.ml
	$(COMPILER) -c $<

clean:
	rm *.cmx *.o *.cmi
	rm splendor
