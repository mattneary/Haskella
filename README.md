# Haskella
A small Haskell dialect written in OCaml with strong OCaml 
influences. The lexer is of course trivial, and the parser
then handles most of the work, elevating expression patterns
to custom OCaml data-structures defined in `syntax.ml`.

Once parsed, an interpreter which looks quite a bit like a
classic Lisp interpreter with error throwing bolted on evaluates
the code's structure.

## Getting Started
All functions need to be explicitly typed, with arguments typed
by an inline postfix notation.

```haskell
not b:bool = if b then false else true
sum x:int y:int = x + y
```

All functions must be typed, including lambdas. The following
utilizes some slightly more complex annotations.

```haskell
> a f:int->int x:int = f x
-: val a : (int -> int) -> int -> int
> a (fun x:int -> x + 1) 2
-: int = 3
```

