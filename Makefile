OCB_FLAGS = -use-ocamlfind -pkgs 'sexplib,str' -I src
OCB = ocamlbuild $(OCB_FLAGS)

all: native

clean:
	$(OCB) -clean

native:
	$(OCB) bach.native

byte:
	$(OCB) bach.byte
