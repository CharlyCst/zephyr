# Grammar specification

The grammar is defined as follow, and parsed using recursive descent.

```
program        -> function* EOF

function       -> "export"? "fun" IDENTIFIER "(" parameters ? ")" result block ";"
parameters     -> IDENTIFIER IDENTIFIER ( "," IDENTIFIER IDENTIFIER)* ","?
result         -> IDENTIFIER?

statement      -> expr_stmt | assign_stmt | let_stmt | if_stmt
                | while_stmt | return_stmt
expr_stmt      -> expression ";"
assign_stmt    -> IDENTIFIER = expression ";"
let_stmt       -> "let" IDENTIFIER = expression ";"
if_stmt        -> "if" expression block ("else" block) ";"
while_stmt     -> "while" expression block ";"
return_stmt    -> "return" expression? ";"

block          -> "{" statement* "}"

expression     -> logical_or
logical_or     -> logical_and ("||" logical_and)*
logical_and    -> equality ("&&" equality)*
equality       -> comparison ( ( "!=" | "==" ) comparison)*
comparison     -> bitwise_or (("<" | ">" | "<=" | ">=") bitwise_or)*
bitwise_or     -> bitwise_and ("|" bitwise_and)*
bitwise_and    -> addition ("&" addition)*
addition       -> multiplication (("+" | "-") multiplication)*
multiplication -> unary (("/" | "*" | "%" ) unary)*
unary          -> (("!" | "-") unary) | call
call           -> primary ( "(" arguments? ")" )*
primary        -> NUMBER | BOOLEAN | IDENTIFIER | "false" | "true"
                | "(" expression ")"
arguments      -> expression ( "," expression )* ","?
```

It is worth noting that there is no semi-colon `;` in Fork, but some are inserted by the scanner following Go-like rules.
