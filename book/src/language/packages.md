# Packages

Fork projects are organised into packages, each package lives in its own folder:

```text
src            # package "http"
├── main.frk
├── cookies.frk
└── request    # package "request"
    ├── headers.frk
    └── status_code.frk
```

There is no constraints on the name of the root package, but all its children **must** have the same name as their directories.

Package can be imported using the `use` keyword which support optionnal aliasing using `as`:

```rust
package "http"

use "http/request"

fun main() {
    request.send()
}
```

If no alias is present, the last part of the package path is used as alias.

A single package may consist of many files, all these files share the same scope, but they **must** all start with the same `package "my_package"` declaration. In the above exemple, functions writen inside of `cookies.frk` can be used inside of `main.frk` without explicitely importing them with the `use` keyword.

To make a declaration (i.e function for now) public it must be preceded by the `pub` keyword:

```rust
package "request"

pub fun send() {
    // Do stuff here
}
```

`pub` functions can be used by other Fork packages, but are not _exposed_ in the final `.wasm` file. They can be if the adequate `expose` statement is present, though.

## Single file package

Sometimes you just want to make a small package to abstract away a few functions, in that case there is no need to create a directory for a single file: in Fork you can create _single file packages_ that lives in the same directory as their parent:

```go
package "http/utils"

// Stuff goes here
```

This file can live in the folder of `http` but is still a separate single file package. However, the file **must** have the same name as the package, in this case it must be named named  `utils.frk`, so that the compiler and most importantly other developers can easily find the file if needed.

## Orphan files

Sometimes you just want to write small scripts, testing can be one of such use cases. Fork offers a notion of _orphan files_ for that purpose, it's like a normal Fork file but its package name must start wirh a `#`:

```go
package "#test_http"
```

There are no other constraints on orphan files, they are discarded by the compiler unless they are the target of compilation.

