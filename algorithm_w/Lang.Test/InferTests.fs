module InferTests

open Xunit

let success expected input =
    match Parser.parseExpr input with
    | Error msg -> Assert.True(false, msg)
    | Ok expr ->
        Infer.reset_id ()
        let ty = Infer.infer Core.core 0 expr
        let generalizedTy = Infer.generalize (-1) ty
        let actual' = generalizedTy |> Expr.string_of_ty
        let expected' = Parser.parseType expected |> Result.map Expr.string_of_ty |> Result.defaultValue "Error"

        if (expected' <> actual') then
            printfn "expr: %s" input
            printfn "expected: %s" expected'
            printfn "actual: %s" actual'

        Assert.Equal(expected', actual')

let failure msg input =
    match Parser.parseExpr input with
    | Error msg -> Assert.True(false, msg)
    | Ok expr ->
        Infer.reset_id ()
        try 
            Infer.infer Core.core 0 expr |> ignore
            Assert.True(false, "expected error, but got: " + input)
        with
            | ex -> Assert.Equal(sprintf "Error \"%s\"" msg, ex.Message)

[<Fact>]
let verifyInfer () = 
    "id" |> success "forall[a] a -> a"
    "one" |> success "int"
    "x" |> failure "variable x not found"
    "let x = x in x" |> failure "variable x not found"
    "let x = id in x" |> success "forall[a] a -> a"
    "let x = fun y -> y in x" |> success "forall[a] a -> a"
    "fun x -> x" |> success "forall[a] a -> a"
    "fun x -> x" |> success "forall[int] int -> int"
    "pair" |> success "forall[a b] (a, b) -> pair[a, b]"
    "pair" |> success "forall[z x] (x, z) -> pair[x, z]"
    "fun x -> let y = fun z -> z in y" |> success "forall[a b] a -> b -> b"
    "let f = fun x -> x in let id = fun y -> y in eq(f, id)" |> success "bool"
    "let f = fun x -> x in let id = fun y -> y in eq_curry(f)(id)" |> success "bool"
    "let f = fun x -> x in eq(f, succ)" |> success "bool"
    "let f = fun x -> x in eq_curry(f)(succ)" |> success "bool"
    "let f = fun x -> x in pair(f(one), f(true))" |> success "pair[int, bool]"
    "fun f -> pair(f(one), f(true))" |> failure "cannot unify types int and bool"
    "let f = fun x y -> let a = eq(x, y) in eq(x, y) in f" |> success "forall[a] (a, a) -> bool"
    "let f = fun x y -> let a = eq_curry(x)(y) in eq_curry(x)(y) in f" |> success "forall[a] (a, a) -> bool"
    "id(id)" |> success "forall[a] a -> a"
    "choose(fun x y -> x, fun x y -> y)" |> success "forall[a] (a, a) -> a"
    "choose_curry(fun x y -> x)(fun x y -> y)" |> success "forall[a] (a, a) -> a"
    "let x = id in let y = let z = x(id) in z in y" |> success "forall[a] a -> a"
    "cons(id, nil)" |> success "forall[a] list[a -> a]"
    "cons_curry(id)(nil)" |> success "forall[a] list[a -> a]"
    "let lst1 = cons(id, nil) in let lst2 = cons(succ, lst1) in lst2" |> success "list[int -> int]"
    "cons_curry(id)(cons_curry(succ)(cons_curry(id)(nil)))" |> success "list[int -> int]"
    "plus(one, true)" |> failure "cannot unify types int and bool"
    "plus(one)" |> failure "unexpected number of arguments"
    "fun x -> let y = x in y" |> success "forall[a] a -> a"
    "fun x -> let y = let z = x(fun x -> x) in z in y" |> success "forall[a b] ((a -> a) -> b) -> b"
    "fun x -> fun y -> let x = x(y) in x(y)" |> success "forall[a b] (a -> a -> b) -> a -> b"
    "fun x -> let y = fun z -> x(z) in y" |> success "forall[a b] (a -> b) -> a -> b"
    "fun x -> let y = fun z -> x in y" |> success "forall[a b] a -> b -> a"
    "fun x -> fun y -> let x = x(y) in fun x -> y(x)" |> success "forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b"
    "fun x -> let y = x in y(y)" |> failure "recursive types"
    "fun x -> let y = fun z -> z in y(y)" |> success "forall[a b] a -> b -> b"
    "fun x -> x(x)" |> failure "recursive types"
    "one(id)" |> failure "expected a function"
    "fun f -> let x = fun g y -> let h = g(y) in eq(f, g) in x" |> success "forall[a b] (a -> b) -> (a -> b, a) -> bool"
    "let const = fun x -> fun y -> x in const" |> success "forall[a b] a -> b -> a"
    "let apply = fun f x -> f(x) in apply" |> success "forall[a b] (a -> b, a) -> b"
    "let apply_curry = fun f -> fun x -> f(x) in apply_curry" |> success "forall[a b] (a -> b) -> a -> b"
