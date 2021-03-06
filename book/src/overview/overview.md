# Compiler Overview

This chapter aims at giving an overview of the compiler, without diving in the details. You will learn how to navigate the project structure, build and use the compiler.

## Project structure

The compiler's repository has the following structure:

```
├── book
├── docs
├── zephyr
├── out
├── src
│   ├── ast
│   ├── error
│   ├── mir
│   └── wasm
├── test
└── test_out
```

**src**  
The compiler sources can be found in the `src` directory, in which live multiple crates. You can learn more about those crates in the dedicated chapters.

**zephyr**  
The `zephyr` folder hosts zephyr source files, they are used for testing and showcase purpose but may be removed in the future. Compiled files can be put inside the `out` folder, `.wasm` files are ignored there.


**test**  
Tests files lives in `test` and their outputs are generated in `test_out`.

**book**  
Finally, this book itself lives inside the compiler repository, its sources can be found in the `book` folder while the produced HTML is hosted in the `docs` folder. The online version of the book is the one inside the `docs` folder of the master branch of the Github repository.
You can find out more information on how to contribute to the book on the [documentation](../contributing/documentation.md) chapter.
