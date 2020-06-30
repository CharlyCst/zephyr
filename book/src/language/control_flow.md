# Control flow

Let's have a look at a more interesting exemple:

```rust
package "pow"

expose pow as _start

fun pow(a i32, b i32) i32 {
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

There a few new things going on here, first we can use aliases when exporting functions with the `as` keyword. The reason I chosed `_start` is because it is the name of the main funtion in runtimes implementing the [WASI](https://hacks.mozilla.org/2019/03/standardizing-wasi-a-webassembly-system-interface/) standard:

```sh
wasmtime pow.wasm 2 5
32
```

To declare variables in Fork we use the `let` keyword. We can also use all the basic control flow primitives:
- `if` and `else`.
- `while` loop, `for` are not yet supported.
- `return` which does what you expect.


