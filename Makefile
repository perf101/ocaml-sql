SHELL=bash

NAME=ocaml-sql
OCAMLFIND=ocamlfind
OCAMLC=$(OCAMLFIND) ocamlopt -c
OCAMLDOC=$(OCAMLFIND) ocamldoc
OCAML_FLAGS=-thread -package postgresql -warn-error
OCAMLC_FLAGS=$(OCAML_FLAGS) +a-4-6-9-27-28-29 -linkpkg
DOC_DIR=doc
OCAMLDOC_FLAGS=$(OCAML_FLAGS) -d $(DOC_DIR) -html -stars -t $(NAME)

.PHONY: all
all: build doc

.PHONY: build
build: sql.cmi sql.cmx

%.cmx: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) $<

.PHONY: doc
doc: sql.mli
	@mkdir -p $(DOC_DIR)
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) $<

.PHONY: clean
clean:
	@rm -rf *.{cmi,cmx,o} $(DOC_DIR)
