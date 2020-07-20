.PHONY: main
main: src/main.rs
	cargo run -- zephyr -o out/hello.wasm

.PHONY: setup
setup:
	printf "\x1b[35m%s\x1b[0m" "Installing runtimes"
	curl https://wasmtime.dev/install.sh -sSf | bash

.PHONY: build
build: src/main.rs
	cargo build

.PHONY: clean
clean:
	rm -f test_out/*.wasm
	cargo clean

.PHONY: test
test: build
	./test.sh

.PHONY: book
book: book/book.toml
	mdbook build book --dest-dir ../docs

.PHONY: serve
serve:
	mdbook watch book --open --dest-dir book/book
