# A basic program

Let's write our first Zephyr program:

```rust
package "main"

expose add 

fun add(a i32, b i32) i32 {
    return a + b
}
```

In Zephyr a file always starts with a `package` declaration, so here we are defining a `main` package. There is no name imposed, it could also have been `add` or anything you like! More on that in a later chapter.

This package exposes a single declaration, an `add` function. Remember that Zephyr targets WebAssembly, which is meant to live inside some runtime, thus we need to expose functions for the runtime to be able tu actually run them.

Then comes to function itself, it has a name, arguments with their types and a return type. Version 0.1.0 only supports `i32`, `i64`, `f32` and `f64`, but structs, strings and more are comming!

Notice that there is no semi-colon `;` in Zephyr, as in Go those are inserted automatically at the right places so that you don't need to bother with that yourself.

Now let's compile our program! For now you will have to compile the compiler yourself, you will need the rust toolchain for that, then run `cargo build --release` at the root of the directory, this will create the binary at `target/release/zephyr`. Then you can compile any zephyr program you like:

```sh
zephyr add.frk
```

This will produce a `main.wasm` file, because we compiled the `main` package. You can choose the output name with the `-o output_name.wasm` option.

Then you need a runtime to actually run a wasm binary, in this exemple we will use [wasmtime](https://github.com/bytecodealliance/wasmtime), but choose the one that floats your boat. With wasmtime we can invoke a wasm function an pass it arguments:

```sh
wasmtime main.wasm --invoke add 3 4
```

And that is it! You just wrote your first Zephyr program!
