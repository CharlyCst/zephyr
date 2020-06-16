# Building the compiler

## Building and compiling

Building the compile is super easy, just run:

```bash
make build
```

It is a simple `cargo run` for now, but this may change in the future.

Compiling a fork (`.frk`) file to wasm is not harder, use `cargo run` and pass the arguments (input file and output file) to the compiler:

```bash
cargo run -- test.frk test.wasm
```

To ease development, the main `make` command compiles `fork/hello.frk` and output the result to `out/hello.wasm`. When working on the compiler, one usually just runs:

```bash
make
```

## Running a compiled file

The compiler outputs binary wasm modules, to run them you need a wasm runtime. A convenient choice is [Wasmtime](https://github.com/bytecodealliance/wasmtime), you can use it as follows:

```bash
wasmtime my_file.wasm [args]
```

## An end to end example

First write some Fork code, for instance:

```rust
// Compute a^b
export fun Main(a i32, b i32) i32 {
    if b == 0 {
        return 1
    }

    let n = b
    let x = a
    let acc = 1

    while n > 1 {
        if n % 2 == 1 {
            acc = acc * x
        }
        x = x * x
        n = n / 2
    }
    return x * acc
}
```

Then compile it:

```bash
cargo run -- pow.frk pow.wasm
```

And run it with Wasmtime (or your favorite wasm runtime):

```bash
wasmtime pow.wasm 5 3
125
```

To test even faster, just put your code inside `fork/hello.frk`, run `make` and then execute `out/hello.wasm`, for instance with `wasmtime out/hello.wasm`, and voilà!
