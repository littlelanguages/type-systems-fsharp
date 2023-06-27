# Extensible Records with Scoped Labels

This is an implementation of type inference for safe, polymorphic and extensible
records.

## Overview

In his paper
[Extensible records with scoped labels](http://research.microsoft.com/apps/pubs/default.aspx?id=65409),
Daan Leijen describes an innovative type inference system for extensible records
which allows duplicate labels in rows. This makes it considerably simpler than
most other record systems, which include predicates on record types such as the
"lacks" predicate _`(r\l)`_, specifying that the record type `r` must not
contain label `l`. This implementation closely follows Daan's presentation in
his paper and is a relatively small extension of the Hindley-Milner type
inference algorithm implemented in (_algorithm_w_)[../algoritm_w].

Records consist of labeled fields with values `{a = one, b = false}` and can
extend other records `{x = false | r}`. The basic operations for records are
_selection_, _extension_ and _restriction_ and are typed as follows:

```
(_.label) : forall[a r] {label : a | r} -> a
{label = _ | _} : forall[a r] (a, {r}) -> {label : a | r}
{_ - label} : forall[a r] {label : a | r} -> {r}
```

## Details

The types of expressions `expr` and types `ty` in [`expr.ml`](./Lang/Expr.fs)
are extended with primitive record operations and types. Records can either be
empty `{}` or extensions of other records `{x = false | r}`. Syntax sugar for
`{x = false | {y = zero | {}}}` is `{x = false, y = zero}`. The type of rows
similarly consists of empty rows `<>` and row extensions `<a : _ | ...>`. A
record type is a wrapper for the type of row; other wrappers could exist (Daan
gives example of sum/variant types).

The core of the type inference is implemented in functions `unify` and
`rewrite_row`. The function `unify` unifies record types by unifying their
enclosing rows, and unifies an empty row only with itself. If a row extension
`<a : t | r>` is unified with another row, the function `rewrite_row` rewrites
the second row by searching for the first field with label `a` and unifies its
type with `t`. All other types are handled as before.

The only other significant change is in function `infer`, where the types of new
expression terms are inferred by treating them as implicit calls to _selection_,
_extension_ and _restriction_ functions with types as above.

## Discussion

One potential problem with this implementation is that record literals and row
types are represented as a list of record/row extensions, whose order depends on
programmer's code and inner workings of the type inference algorithm. The
unification procedure can rearrange fields as necessary, but records and record
types can not be easily compared or canonically represented by strings. A better
solution would be to gather all labels into a multi-map and use a specific
sorting order for labels when representing rows as strings (implemented in
[**extensible_rows2**](https://github.com/tomprimozic/type-systems/tree/master/extensible_rows2)).

While this type system is simple to implement and use, for example, it is a part
of the language [Elm](https://guide.elm-lang.org/core_language.html), it
represents only one possibility for typing extensible records. Other proposals,
summarized in
[GHC wiki](https://ghc.haskell.org/trac/ghc/wiki/ExtensibleRecords), include
first-class labels, positive and negative ("lacks") predicates for record types
and even more general predicates such as "disjoint", and also include structural
subtyping (as used for objects in OCaml and Go).

# References

- [Tom Primozic](https://github.com/tomprimozic/type-systems). _Type Systems_.
  2014
