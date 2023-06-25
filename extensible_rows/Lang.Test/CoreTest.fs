module CoreTests

open Xunit
open Expr
open Core

let private success expected name =
    let actual' = core.[name] |> string_of_ty

    if (expected <> actual') then
        printfn "name: %s" name
        printfn "expected: %s" expected
        printfn "actual: %s" actual'
        printfn "--- raw ----"
        printfn "%A" core.[name]
        printfn "------------"

    Assert.Equal(expected, actual')

[<Fact>]
let verifyCore () =
    "head" |> success "forall[a] list[a] -> a"
    "tail" |> success "forall[a] list[a] -> list[a]"
    "nil" |> success "forall[a] list[a]"
    "cons" |> success "forall[a] (a, list[a]) -> list[a]"
    "cons_curry" |> success "forall[a] a -> list[a] -> list[a]"
    "map" |> success "forall[a b] (a -> b, list[a]) -> list[b]"
    "map_curry" |> success "forall[a b] (a -> b) -> list[a] -> list[b]"
    "one" |> success "int"
    "zero" |> success "int"
    "succ" |> success "int -> int"
    "plus" |> success "(int, int) -> int"
    "eq" |> success "forall[a] (a, a) -> bool"
    "eq_curry" |> success "forall[a] a -> a -> bool"
    "not" |> success "bool -> bool"
    "true" |> success "bool"
    "false" |> success "bool"
    "pair" |> success "forall[a b] (a, b) -> pair[a, b]"
    "pair_curry" |> success "forall[a b] a -> b -> pair[a, b]"
    "first" |> success "forall[a b] pair[a, b] -> a"
    "second" |> success "forall[a b] pair[a, b] -> b"
    "id" |> success "forall[a] a -> a"
    "const" |> success "forall[a b] a -> b -> a"
    "apply" |> success "forall[a b] (a -> b, a) -> b"
    "apply_curry" |> success "forall[a b] (a -> b) -> a -> b"
    "choose" |> success "forall[a] (a, a) -> a"
    "choose_curry" |> success "forall[a] a -> a -> a"
