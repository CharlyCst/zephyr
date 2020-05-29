# Architecture

For now the compiler is composed of a few crates, namely `ast`, `mir`, `wasm` and `error`. Others will be created as needed in the future.

## The compiler pipeline

When compiling source code, the compiler goes through the following stages of the pipeline:

- AST, for Abstract Syntax Tree, this stage is responsible for scanning the source code and parsing, it creates an abstract syntax tree ready to be used by the next stage.
- MIR, or mid level intermediate representation (because we may need an high level intermediate representation in the future ;) ). Its first perform a name resolution step on the AST, that is it looks for name definitions and gives unique IDs for each of them. The name resolution step is also used to generate type constraints, these are used for type checking just after name resolution. Once the program is successfully type-checked, mir-code is emitted, it looks almost like wasm with some minor differences.
- WASM, the last step that translates mir to the binary wasm representation, ready to be executed.

## Error handling

Errors can happen at any stage of the compiler, reporting those errors and helping the developer to solve them is at least as important as compiling correct code.

To produce high quality errors, each stage of the pipeline (and each substage) is given an `ErrorHandler` whose job is to store errors. Anywhere in the pipeline the compilation can be aborted and error reported (it is not mandatory to stop compilation as soon as an error occurs).

To generate helpful messages the `ErrorHandler` uses `Location`s, those are composed of a position (in character offset from the beginning of the file) and a length. Errors are stored with their location, and once the error handler is asked to report the errors the source file is scanned again to get back the lines and erroneous source code.
