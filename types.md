# Type system

Zephyr currently support the following types:

```
i64
i32
f64
f32
bool
```

The four types `i32`, `i64`, `f32`, `f64` are wasm built-in, whereas other types are built in top of them.

For now functions are the only composite type:

```
fun(T_1, T_2, ...) T_3
fun(T_1, T_2, ...)
```

```
// Statements

Γ |- y: T    x = y
------------------
Γ |- x: T


Γ |- e: T    "let" x = e
------------------------
Γ |- x: T
 

"if" e block_stmt
-----------------
|- e: bool


"while" e block_stmt
--------------------
|- e: bool


Γ |- f: fun(T_1, ...) T    f(...) { return e }
----------------------------------------------
Γ |- e: T


// Expressions

if eq := "==" | "!=" 

Γ |- y: T    x eq y
-------------------
Γ |- x: T


if cmp := "<" | ">" | "<=" | ">="

Γ |- y: T    x cmp y
--------------------
Γ |- x: T, T ∈ {i32, i64, f32, f64}


if op != "+" | "-" | "*" | "/"

Γ |- y: T    x op y
-------------------
Γ |- x: T, T ∈ {i32, i64, f32, f64}


!x
----------
|- x: bool


-x
---------------------------------
|- x: T, T ∈ {i32, i64, f32, f64}
```
 
