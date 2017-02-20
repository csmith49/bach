# ==> BUILD FLAGS
OCB_FLAGS = -r -use-ocamlfind -pkgs 'sexplib,str,ppx_sexp_conv'  -I src -tag 'debug'
OCB = ocamlbuild $(OCB_FLAGS)

# ==> BENCHMARKING UTILITIES

# locations and folders
TOOLS = tools
EPEE = $(TOOLS)/epee/epee.py
GRAPHS = graphs
TRUTH = truth

# constants for benchmarking functions
NUM_FACTS = 2000

GRAPH_TIMEOUT = 300
GRAPH_BM = finitefield
GRAPH_GRAMMAR = benchmarks/$(GRAPH_BM)/$(GRAPH_BM).sexp
GRAPH_FACTS = benchmarks/$(GRAPH_BM)/facts
GRAPH_SIZES = 10 50 100 500 1000

TRUTH_START = 1000
TRUTH_SIZES = 10 50 100 500
TRUTH_DEPTH = 7
TRUTH_ROUNDS = 5

# functions for calling particular tools
define gen_facts
	python3 $(EPEE) benchmarks/$(1)/$(1).py -o benchmarks/$(1)/facts -c $(NUM_FACTS)

endef

define gen_graph_data
	gtimeout $(GRAPH_TIMEOUT) ./bach.native -induct $(GRAPH_GRAMMAR) -fact $(GRAPH_FACTS)/ -times -sample $(1) | tee $(GRAPHS)/d_$(1).csv

endef

define gen_truth
	bach.native -b $(1) -interval $(TRUTH_START) $(NUM_FACTS) -maxdepth $(TRUTH_DEPTH) -csv | tee $(TRUTH)/$(1).csv

endef

define check_truth
	@echo --> Checking $(2) with fact size $(1)
	python3 $(TOOLS)/check_error.py -g $(TRUTH)/$(2).csv -s $(1) -i 0 $(TRUTH_START) -b $(2) -d $(TRUTH_DEPTH) -r $(TRUTH_ROUND)

endef
# benchmarking identifiers
BENCHMARKS = dict finitefield geometry lists matrix queue sets strings trig

# ==> RULES
all: native

clean:
	$(OCB) -clean

native:
	$(OCB) bach.native

byte:
	$(OCB) bach.byte

facts:
	$(foreach bm,$(BENCHMARKS),$(call gen_facts,$(bm)))

graph:
	$(foreach gs,$(GRAPH_SIZES),$(call gen_graph_data,$(gs)))
	python3 $(TOOLS)/gen_graph.py $(foreach gs,$(GRAPH_SIZES),$(GRAPHS)/d_$(gs).csv)

truth:
	$(foreach bm,$(BENCHMARKS),$(call gen_truth,$(bm)))

error:
	$(foreach bm,$(BENCHMARKS),$(foreach ts,$(TRUTH_SIZES),$(call check_truth,$(ts),$(bm))))
