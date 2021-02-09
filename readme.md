<div align="center">

<h1>Zephyr</h1>

<div align="center">
  <img src="./assets/zephyr_transparent.png"/>
</div>

<strong>A language built for WebAssembly</strong>

</div>

You are early to the party 🎉 Zephyr is still a work in progress, feel free to try it out though!

Zephyr is a language that compiles to WebAssembly, its main goal is to showcase the concept of `runtime interfaces` as a language feature to allow the exact same code to run on a wide variety of WebAssembly runtimes, or even to behave differently on the same runtime by choosing the underlying implementation of runtime interfaces.

For instance what should `print` do when compiling to WebAssembly? It depends.
When working on a CLI it should write to stdout, on the web to the console, on an arduino to an LCD screen or maybe send the logs back over the wire.

Zephyr will eventually define a standard `Printer` runtime interface and let you choose or write the implementation that works for you, this way _you_ can choose how your code behave without waiting for someone to extend the compiler to support your use case.

## Status

| feature                     |     |
| --------------------------- | --- |
| Type inference              |  ✔️  |
| Package system              |  ✔️  |
| Runtime packages            |  ✔️  |
| Runtime interfaces          |  ⏳ |
| Memory allocator            |  ✔️  |
| Automatic memory management |  ⏳ |
| Structs                     |  🚧 |
| Tuples (product types)      |  ⏳ |
| Sum types                   |  ⏳ |


## Trying Zephyr

Zephyr is still a work in progress and currently lacks some major features, but still you can try it out.

First clone this repository:

```bash
git clone git@github.com:CharlyCst/zephyr.git
cd zephyr 
```

Some programs may need the Zephyr standard library, you must set the `ZEPHYR_LIB` environment variable to the location of the `lib` folder for them to compile properly:

```bash
export ZEPHYR_LIB=`pwd`/lib
```

If you plan on using Zephyr on a regular basis, add this export to your `.bashrc` (or equivalent).

Then write a Zephyr program:

```rust
standalone package hello

use std.r.wasi

expose hello as _start

fun hello() {
    wasi.print("Hello, world!\n")
}
```

Compile it

```bash
cargo run -- hello.zph
```

And run it with your favorite WASM runtime, for instance [Wastime](https://github.com/bytecodealliance/wasmtime)

```bash
wasmtime hello.zph.wasm
Hello, world!
```

