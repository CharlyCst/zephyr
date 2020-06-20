# Grammar specification

## Main grammar

The grammar is defined as follow, and parsed using recursive descent.

```
program        -> package declaration* EOF

package        -> "package" STRING ";"

declaration    -> use | expose | function
use            -> "use" STRING ( "as" IDENTIFIER)? ";"
expose         -> "expose" IDENTIFIER ("as" IDENTIFIER)? ";"
function       -> "pub"? "fun" IDENTIFIER "(" parameters ? ")" result block ";"

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
call           -> access ( "(" arguments? ")" )*
access         -> primary ( "." primary )*
primary        -> NUMBER | BOOLEAN | IDENTIFIER | "false" | "true"
                | "(" expression ")"
arguments      -> expression ( "," expression )* ","?
```

It is worth noting that there is no semi-colon `;` in Fork, but some are inserted by the scanner following Go-like rules.

**Priority tree:**

```
                         [||]
                          ↓
                         [&&]
                          ↓
                      [==] [!=]
                          ↓
                   [<] [>] [<=] [>=]
                          ↓
                         [|]
                          ↓
                         [&]
                          ↓
                       [+] [-]
                          ↓
                     [/] [*] [%]
                          ↓
                       [!] [-]
                          ↓
                        [call]
                          ↓
[0-9] [true,false] [ident] ['(' expression ')']
```

## Fasm grammar

This grammar is used by the fasm front end, it is much simpler and closer to wasm. Its main purpose is to ease the use of low level wasm instructions needed for the runtime and standard library.

```
program -> package declaration\* EOF

package -> "package" STRING ";"
declaration -> expose | function
expose -> "expose" IDENTIFIER ("as" IDENTIFIER)? ";"
function -> "pub"? "fun" IDENTIFIER "(" parameters ? ")" result block ";"
parameters -> IDENTIFIER IDENTIFIER ( "," IDENTIFIER IDENTIFIER)\* ","?
result -> IDENTIFIER?

block -> "{" statement\* "}"
statement -> opcode primary? ";"
primary -> NUMBER | IDENTIFIER

opcode -> WASM_OPCODE
```
