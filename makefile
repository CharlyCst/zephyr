.PHONY: main
main: src/main.rs
	cargo run -- fork/hello.frk out/hello.wasm

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