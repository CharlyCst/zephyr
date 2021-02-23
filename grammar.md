# Grammar specification

## Main grammar

The grammar is defined as follow, and parsed in recursive descent fashion.

```
program        -> package declaration* EOF

package        -> "standalone"? "runtime"? "package" IDENTIFIER ";"

declaration    -> use | expose | function | struct | imports
use            -> "use" path ( "as" IDENTIFIER)? ";"
expose         -> "expose" IDENTIFIER ("as" IDENTIFIER)? ";"
imports        -> "from" IDENTIFIER "import" import_block ";"
function       -> "pub"? "fun" IDENTIFIER "(" parameters ? ")" result block ";"
struct         -> "pub"? struct IDENTIFIER struct_block  ";"

import_block   -> "{" import* "}"
import         -> "pub"? "fun" IDENTIFIER "(" parameters ? ")" result ("as" IDENTIFIER) ";"

struct_block   -> "{" ( struct_field ( ("," | ";") struct_field )* ("," | ";")? )? "}"
struct_field   -> "pub"? IDENTIFIER ":" type

parameters     -> IDENTIFIER ":" type ( "," IDENTIFIER ":" type )* ","?
result         -> (":" type)?

statement      -> expr_stmt | assign_stmt | let_stmt | if_stmt
                | while_stmt | return_stmt
expr_stmt      -> expression ";"
assign_stmt    -> expression = expression ";"
let_stmt       -> "let" IDENTIFIER = expression ";"
if_stmt        -> "if" expression¹ block ("else" block) ";"
while_stmt     -> "while" expression¹ block ";"
return_stmt    -> "return" expression? ";"

block          -> "{" statement* "}"

expression     -> logical_or
logical_or     -> logical_and ("||" logical_and)*
logical_and    -> equality ("&&" equality)*
equality       -> comparison ( ( "!=" | "==" ) comparison)*
comparison     -> bitwise_or (("<" | ">" | "<=" | ">=") bitwise_or)*
bitwise_or     -> bitwise_xor ("|" bitwise_xor)*
bitwise_xor    -> bitwise_and ("^" bitwise_and)*
bitwise_and    -> addition ("&" addition)*
addition       -> multiplication (("+" | "-") multiplication)*
multiplication -> unary (("/" | "*" | "%" ) unary)*
unary          -> (("!" | "-") unary) | call
call           -> access ( "(" arguments? ")" )*
access         -> primary ( "." primary )*
primary        -> INTEGER | FLOAT | BOOLEAN | STRING | IDENTIFIER
                | struct_literal | "false" | "true" | "(" expression ")"
                | "(" ( expression "," )+ expression? ")"

arguments      -> ( expression ( "," expression )* ","? )?
struct_literal -> IDENTIFIER "{" (field ( ("," | ";") field )* ("," | ";")?)? "}"
field          -> IDENTIFIER ( ":" expression )?

path           -> IDENTIFIER ( "." IDENTIFIER )*
type           -> path | "(" type ( "," type )* ","? ")"

// expression¹: except `struct_literal`, but `struct_literal` are allowed inside parentheses.
```

It is worth noting that there is no semi-colon `;` in Zephyr, but some are inserted by the scanner following Go-like rules.

## Zephyr Assembly (zasm) grammar

This grammar is used by the zasm front end, it is much simpler and closer to wasm. Its main purpose is to ease the use of low level wasm instructions needed for the runtime and standard library.

```
program     -> package declaration* EOF

package     -> "package" IDENTIFIER ";"
declaration -> expose | function
expose      -> "expose" IDENTIFIER ("as" IDENTIFIER)? ";"
function    -> "pub"? "fun" IDENTIFIER "(" parameters ? ")" result block ";"
parameters  -> IDENTIFIER ":" IDENTIFIER ( "," IDENTIFIER ":" IDENTIFIER)* ","?
result      -> (":" type)?

block       -> "{" statement* "}"
statement   -> opcode primary? ";"
primary     -> NUMBER | IDENTIFIER

type        -> IDENTIFIER | "(" IDENTIFIER ( "," IDENTIFIER )* ","? ")"
opcode      -> WASM_OPCODE
```
