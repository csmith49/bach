OCB_FLAGS = -r -use-ocamlfind -pkgs 'sexplib,str,ppx_sexp_conv'  -I src -tag 'debug'
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) bach.native

byte:
	$(OCB) bach.byte
