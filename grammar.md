# Grammar specification

The grammar is defined as follow, and parsed using recursive descent.

```
program        -> statement* EOF

statement      -> expr_stmt
expr_stmt      -> expression ";"

expression     -> logical_or
logical_or     -> logical_and ("||" logical_and)*
logical_and    -> equality ("&&" equality)*
equality       -> comparison ( ( "!=" | "==" ) comparison)*
comparison     -> bitwise_or (("<" | ">" | "<=" | ">=") bitwise_or)*
bitwise_or     -> bitwise_and ("|" bitwise_and)*
bitwise_and    -> addition ("&" addition)*
addition       -> multiplication (("+" | "-") multiplication)*
multiplication -> unary (("/" | "*") unary)*
unary          -> (("!" | "-") unary) | primary
primary        -> NUMBER | "false" | "true" | "(" expression ")"
```

It is worth noting that there is no semi-colon `;` in Fork, but some are inserted by the scanner following Go-like rules.
