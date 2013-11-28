# Haskella
A small Haskell dialect written in OCaml with strong OCaml 
influences. The lexer is of course trivial, and the parser
then handles most of the work, elevating expression patterns
to custom OCaml data-structures defined in `syntax.ml`.

Once parsed, an interpreter which looks quite a bit like a
classic Lisp interpreter with error throwing bolted on evaluates
the code's structure.

## Getting Started
All functions need to be explicitly typed, and `let` is used, as in
OCaml, in the definition of functions.

```haskell
let not = fun b : bool -> if b then false else true
```

