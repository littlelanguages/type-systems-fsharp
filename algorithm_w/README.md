# Algorithm W

Algorithm W is the original algorithm for infering types in the
Damas-Hindley-Milner type system. It supports polymorphic types, such as
`forall[a] a -> a`, let generalization, and infers principal (most general)
types. Although it is formally described using explicit substitutions, it
permits an efficient implemenation using updatable references, achieving close
to linear time complexity (in terms of the size of the expression being
type-infered).

## Overview

For a general description of Algorithm W, see the
[Wikipedia article](http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W)
[2]. This implementation uses several optimizations over the naive
implementation. Instead of explicit substitutions when unifying types, it uses
updateable references. It also tags unbound type variables with levels or ranks
to optimize generalizations of let bindings, a technique first described by
Didier Rémy [1]. A very eloquent description of the ranked type variables
algorithm and associated optimizations was written by
[Oleg Kiselyov](http://okmij.org/ftp/ML/generalization.html) [3].

## Details

The basic terms of Lambda Calculus and the structure of the types are defined in
[`Expr.fs`](./Lang/Expr.fs). The parser is implemented in
[`Parser.fs`](./Lang/Parser.fs) using
[FParsec](https://www.quanttec.com/fparsec/). The main type inference is
implemented in the file [`Infer.fs`](./Lang/Infer.fs).

The function `infer` takes an environment, a level used for let-generalization,
and an expression, and infers types for each term type. _Variables_ are looked
up in the environment and instantiated. The type of _functions_ is inferred by
adding the function parameters to the type environment using fresh type
variables, and inferring the type of the function body. The type of _let_
expression is inferred by first inferring the type of the let-bound value,
generalizing the type, and the inferring the type of the let body in the
extended type environment. Finally, the type of a _call_ expression is inferred
by first matching the type of the expression being called using the
`match_fun_ty` function, and then inferring the types of the arguments and
unifying them with the types of function parameters.

The function `unify` takes two types and tries to _unify_ them, i.e. determine
if they can be equal. Type constants unify with identical type constants, and
arrow types and other structured types are unified by unifying each of their
components. After first performing an "occurs check", unbound type variables can
be unified with any type by replacing their reference with a link pointing to
the other type.

The function `occurs_check_adjust_levels` makes sure that the type variable
being unified doesn't occur within the type it is being unified with. This
prevents the algorithm from inferring recursive types, which could cause
naively-implemented type checking to diverge. While traversing the type tree,
this function also takes care of updating the levels of the type variables
appearing within the type, thus ensuring the type will be correctly generalized.

Function `generalize` takes a level and a type and turns all type variables
within the type that have level higher than the input level into generalized
(polymorphic) type variables. Function `instantiate` duplicates the input type,
transforming any polymorphic variables into normal unbound type variables.

# Possible optimizations

Although this implementation is reasonably efficient, state-of-the-art
implementations of HM type inference employ several more advanced techniques
which were avoided in this implementation for the sake of clarity. As outlined
in Oleg's article, OCaml's type checker marks _every_ type with a type level,
which is the maximum type level of the type variables occuring within it, to
avoid traversing non-polymorphic types during instantiation. It also delays
occurs checks and level adjustments. It can deal with recursive types, which
arise often while type checking objects, by marking types during unification.
Oleg describes a further optimization that could be performed by condensing the
sequences of type links as we follow them, but our `generalize` function takes
care of that problem.

# References

- [1] Didier Rémy. _Extending ML type system with a sorted equational theory._
  1992
- [2] [wikipedia](http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W).
  _Hindley-Milner Type System: Algorithm W_
- [3] [Oleg Kiselyov](http://okmij.org/ftp/ML/generalization.html). _How OCaml type
  checker works -- or what polymorphism and garbage collection have in common_.
  2022
- [4] [Tom Primozic](https://github.com/tomprimozic/type-systems). _Type Systems_.
  2014
