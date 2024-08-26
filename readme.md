# Interpreter made with OCaml

Interpreter of a functional language made with OCaml.

## Description

This project delves into the implementation of a functional language interpreter.
Here are its key aspects:

- Lambda Calculus: Define and utilize anonymous functions for flexible code organization.
- Pattern Matching: Deconstruct data and control program flow through elegant pattern matching on data structures.

- Type System: The interpreter includes a type system to ensure type safety and catch errors at compile time.
  - Type Inference: The interpreter uses type inference by type constraints and unification to infer types for variables and expressions with implicit types.

- Call-by-Name (CBN) Evaluation: The interpreter highlights CBN evaluation, enabling:
  - Manipulation of Infinite Data: Represent and process infinite data structures like infinite lists.
  - Lazy Evaluation: Expressions are evaluated only when their values are needed, potentially improving performance.
- Call-by-Value (CBV) Evaluation: A separate implementation of the interpreter using the more traditional CBV strategy.

This project serves as a practical example for understanding functional language implementation, type systems, and the nuances of different evaluation strategies.

ocamllex: Lexical analysis for accurate tokenization of source code.
menhir: Parser generation to transform tokens into an abstract syntax tree for interpretation.

## Build

Requires OCaml >=4.13.1, dune, and menhir.

Install OCaml
> <https://ocaml.org/install>

Install packages (dune, menhir).

```bash
opam install . --deps-only 
```

Navigate to the project directory where dune-project is located. Then build with dune.

```bash
cd <project_directory>
dune build
```

## usage

Use dune to run.
There are two implementations which use different evaluation strategy.

```bash
dune exec interpreter # call by value version
dune exec cbn # call by name version
```

This will get input from stdin and print the result to stdout.

```bash
dune exec interpreter [filenames...] # get inputs from files
```

This will get input from files and print the result to stdout.

```bash
dune test # run tests
```

This will run tests with files in tests foo.in as input and foo.exp as expected output.

## Examples

<!-- TODO : more description -->
<!-- TODO : use asciinema? -->

### 1. Basic expressions

```pseudo
1 + 1;;
- : int = 2 

let x0 = true;; 
val x0 : bool = true  (* CBV *)
val x0 : bool = <thunk> (* CBN *)

let x1 = 1 in 
let x2 = 2 in 
if false < x0 then 6 / 2 + x1 * (2 + x2) else 0;;
- : int = 7 

(fun x y -> x + 1) 1 3;;
- : int = 2
```

### 2. Mutually Recursive Functions

```pseudo
let rec even x = if x = 0 then true else odd (x - 1)
and odd x = if x = 0 then false else even (x - 1) in
even 43;;
- : bool = false
```

### 3. Higher-Order Functions and Type Inference

```pseudo
let rec map f x = match x with 
    [] -> []
    | x::xs -> f x :: (map f xs) end;;
val map : (6' -> 5') -> 6' list -> (5' list) = <rfun>

let plus1 =  map (fun x -> x + 1);;
val plus1 : int list -> (int list) = <fun>

plus1 [1;2;3];;
- : int list = [2;3;4]
```

### 4. Infinite Data Structuers Using CBN

```pseudo
let rec ones = 1 :: ones in 
let rec take n l = match l with
    x::xs -> if n = 1 then x::[] else x::(take (n -1) xs)
    | [] -> [] end in
take 5 ones;;
- : int list = [1;1;1;1;1]
```

## Syntax

Simplified description of the syntax.

- `<program>` is the start symbol.
- `<expr>` is the expression.
- `{<foo>}` means zero or more of `<foo>`

```bnf
<program> ::= 
    | {<comm>} end_of_file

<comm> ::=
    | <expr> ";;"
    | "let" <var> "=" <expr> ";;"
    | "let" "rec" <var> {<var>} "=" <expr> ";;"
    | "let" "rec" <var> {<var>} "=" <expr> "and" <var> {<var>} "=" <expr> {<var> {<var>} "=" <expr>} ";;"

<expr> ::= 
    | <atomic_expr>
    | "(" <expr> ")"
    | <expr> <bin_op> <expr> 
    | "-" <expr>
    | "fun" <var> {<var>} "->" <expr>                        
    | <expr> <expr>
    | "if" <expr> "then" <expr> "else" <expr>
    | "let" <var> {<var>} "=" <expr> "in" <expr>           
    | "let" "rec" <var> {<var>} "=" <expr> "in" <expr>      
    | "let" "rec" <var> {<var>} "=" <expr> "and" <var> {<var>} "=" <expr> {<var> {<var>} "=" <expr>} "in" <expr> 
    | "match" <expr> "with" <pattern_matching> "end"
    | <expr> "," <expr> 
    | <expr> "::" <expr>
    | "[" <expr> ";" <expr> {";" <expr>}  "]"

<pattern_matching> ::=
    | <pattern> "->" <expr> {"|" <pattern> "->" <expr>}

<pattern> ::=
    | <var>
    | <int>
    | <bool>
    | "_"
    | "(" <pattern> ")"
    | <pattern> "," <pattern>
    | <pattern> "::" <pattern>
    | "[" <pattern> ";" <pattern> {";" <pattern>}  "]"
    | "[" "]"

<bin_op> ::= 
    | "+" 
    | "-" 
    | "*" 
    | "/" 
    | "<"
    | "="

<atomic_expr> ::=
    | <int>
    | <bool>
    | <var> 
    | "[" "]"  

<int> ::= <digit> {<digit>}

<bool> ::= 
    | "true"
    | "false"

<var> ::= <alpha> {<alpha> | <digit>}

<alpha> ::= "a" | ... | "z" | "A" | ... | "Z" | "_"

<digit> ::= "0" | ... | "9"
```
