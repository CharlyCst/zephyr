<div align="center">

<h1>Zephyr</h1>

<div align="center">
  <img src="./assets/zephyr.png"/>
</div>

<strong>A language built for WebAssembly</strong>

</div>

You are early to the party 🎉 Zephyr is still a work in progress, feel free to try it out though!

Zephyr is a language that compiles to WebAssembly, it aims at being **very portable** and **easy to integrate** with other wasm modules, possibly written in other languages.

To achieve these goals, the following features are currently explored:

- **Small runtime**, exploring a Rust-style automatic memory management system (no GC)
- **Two module levels**: Zephyr-level imports/exports and WASM-level imports/exports
- Introduce the notion of host **Runtime**, allowing to expose interfaces with multiple underneath implementations using host runtime specific hooks (starting with Web and WASI)

## Trying Zephyr

Zephyr is still a work in progress and currently lacks some major features, but still you can try it out.

First clone this repository:

```bash
git clone git@github.com:CharlyCst/zephyr.git
cd zephyr 
```

Then write a Zephyr program:

```rust
package "pow"

expose pow 

fun pow(a: i32, b: i32): i32 {
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

Compile it

```bash
cargo run -- pow.zph pow.wasm
```

And run it with your favorite WASM runtime, for instance [Wastime](https://github.com/bytecodealliance/wasmtime)

```bash
wasmtime pow.wasm --invoke pow 5 3
125
```

