# build flags
OCB_FLAGS = -r -use-ocamlfind -pkgs 'sexplib,str,ppx_sexp_conv'  -I src -tag 'debug'
OCB = ocamlbuild $(OCB_FLAGS)

# fact gen functionality
NUM_FACTS = 1000
EPEE = tools/epee/epee.py

define facts
	python3 $(EPEE) benchmarks/$(1)/$(1).py -o benchmarks/$(1)/facts -c $(NUM_FACTS)

endef

# benchmarking stuff
BENCHMARKS = dict finitefield geometry lists matrix queue sets strings trig

# rules
all: native

clean:
	$(OCB) -clean

native:
	$(OCB) bach.native

byte:
	$(OCB) bach.byte

facts:
	$(foreach bm,$(BENCHMARKS),$(call facts,$(bm)))
