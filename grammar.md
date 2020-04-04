# Grammar specification

```
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
