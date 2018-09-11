.PHONY: all
all: build doc

.PHONY: build
build:
	dune build --profile release

.PHONY: doc
doc:
	dune build @doc-private --profile release
