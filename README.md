# type-systems-fsharp

Implementing type inference is tricky - there are multiple type systems, and each has its own quirks. This repository contains a collection of type systems implemented in F#.  I have tried to keep the implementations as simple as possible, so that they can be used as a starting point for further experimentation.

I would like to shout out to a number of people who I have pilfered code from.  The code in this repository is based on the following sources:

- Tom Primozic's repo https://github.com/tomprimozic/type-systems has been fabulous.
- Stephen Diehl's repo https://github.com/sdiehl/write-you-a-haskell has been a great resource.

## Implementations

The implementations contained in these repos are built with the intention that they can be incorporated into other projects.  They are not intended to be run standalone.

- [algorithm_w](./algorithm_w) contains one of the most basic yet efficient implementation of Damas-Hindley-Milner type inference algorithm (used in functional languages such as OCaml, Haskell and Elm) called Algorithm W. Uses references to simulate type substitutions and assigns ranks/levels to type variables to simplify let-generalization.  This implementation is an F# version of Tom Primozic's effort.
