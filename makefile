export ZEPHYR_LIB=$(shell pwd)/lib

.PHONY: help
help:
	@echo "Commands: setup, build, clean, test"

.PHONY: setup
setup:
	printf "\x1b[35m%s\x1b[0m" "Installing runtimes"
	curl https://wasmtime.dev/install.sh -sSf | bash

.PHONY: build
build:
	cargo build

.PHONY: clean
clean:
	rm -f test_out/*.wasm
	cargo clean

.PHONY: test
test: build
	python3 tests.py

.PHONY: book
book: book/book.toml
	mdbook build book --dest-dir ../docs

.PHONY: serve
serve:
	mdbook watch book --open --dest-dir book/book
