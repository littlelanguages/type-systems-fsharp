module InferTests

open Xunit

let private success expected input =
    match Parser.parseExpr input with
    | Error msg -> Assert.True(false, msg)
    | Ok expr ->
        Infer.reset_id ()
        let ty = Infer.infer Core.core 0 expr
        let generalizedTy = Infer.generalize (-1) ty
        let actual' = generalizedTy |> Expr.string_of_ty

        let expected' =
            match Parser.parseType expected |> Result.map Expr.string_of_ty with
            | Ok expected' -> expected'
            | Error msg -> "Error: " + msg

        if (expected' <> actual') then
            printfn "expr: %s" input
            printfn "ty: %A" (Parser.parseType expected)
            printfn "expected: %s" expected'
            printfn "actual: %s" actual'

        Assert.Equal(expected', actual')

let private failure msg input =
    match Parser.parseExpr input with
    | Error msg -> Assert.True(false, msg)
    | Ok expr ->
        Infer.reset_id ()

        try
            Infer.infer Core.core 0 expr |> ignore
            Assert.True(false, "expected error, but got: " + input)
        with ex ->
            Assert.Equal(sprintf "Error \"%s\"" msg, ex.Message)

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

    "let f = fun x y -> let a = eq(x, y) in eq(x, y) in f"
    |> success "forall[a] (a, a) -> bool"

    "let f = fun x y -> let a = eq_curry(x)(y) in eq_curry(x)(y) in f"
    |> success "forall[a] (a, a) -> bool"

    "id(id)" |> success "forall[a] a -> a"
    "choose(fun x y -> x, fun x y -> y)" |> success "forall[a] (a, a) -> a"
    "choose_curry(fun x y -> x)(fun x y -> y)" |> success "forall[a] (a, a) -> a"
    "let x = id in let y = let z = x(id) in z in y" |> success "forall[a] a -> a"
    "cons(id, nil)" |> success "forall[a] list[a -> a]"
    "cons_curry(id)(nil)" |> success "forall[a] list[a -> a]"

    "let lst1 = cons(id, nil) in let lst2 = cons(succ, lst1) in lst2"
    |> success "list[int -> int]"

    "cons_curry(id)(cons_curry(succ)(cons_curry(id)(nil)))"
    |> success "list[int -> int]"

    "plus(one, true)" |> failure "cannot unify types int and bool"
    "plus(one)" |> failure "unexpected number of arguments"
    "fun x -> let y = x in y" |> success "forall[a] a -> a"

    "fun x -> let y = let z = x(fun x -> x) in z in y"
    |> success "forall[a b] ((a -> a) -> b) -> b"

    "fun x -> fun y -> let x = x(y) in x(y)"
    |> success "forall[a b] (a -> a -> b) -> a -> b"

    "fun x -> let y = fun z -> x(z) in y"
    |> success "forall[a b] (a -> b) -> a -> b"

    "fun x -> let y = fun z -> x in y" |> success "forall[a b] a -> b -> a"

    "fun x -> fun y -> let x = x(y) in fun x -> y(x)"
    |> success "forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b"

    "fun x -> let y = x in y(y)" |> failure "recursive types"
    "fun x -> let y = fun z -> z in y(y)" |> success "forall[a b] a -> b -> b"
    "fun x -> x(x)" |> failure "recursive types"
    "one(id)" |> failure "expected a function"

    "fun f -> let x = fun g y -> let h = g(y) in eq(f, g) in x"
    |> success "forall[a b] (a -> b) -> (a -> b, a) -> bool"

    "let const = fun x -> fun y -> x in const" |> success "forall[a b] a -> b -> a"
    "let apply = fun f x -> f(x) in apply" |> success "forall[a b] (a -> b, a) -> b"

    "let apply_curry = fun f -> fun x -> f(x) in apply_curry"
    |> success "forall[a b] (a -> b) -> a -> b"

    "{}" |> success "{}"
    "{}.x" |> failure "cannot unify types {x : _1 | _0} and {}"
    "{a = one}" |> success "{a : int}"
    "{a = one, b = true}" |> success "{a : int, b : bool}"
    "{b = true, a = one}" |> success "{b : bool, a : int}"
    "{a = one, b = true}.a" |> success "int"
    "{a = one, b = true}.b" |> success "bool"
    "{a = one, b = true}.c" |> failure "row does not contain label c"
    "{f = fun x -> x}" |> success "forall[a] {f : a -> a}"
    "let r = {a = id, b = succ} in choose(r.a, r.b)" |> success "int -> int"
    "let r = {a = id, b = fun x -> x} in choose(r.a, r.b)" |> success "forall[a] a -> a"
    "choose({a = one}, {})" |> failure "cannot unify types {a : int} and {}"
    "choose({a = one, b = true}, {b = true, a = one})" |> success "{a : int, b : bool}"
    "{ x = zero | { y = one | {} } }" |> success "{x : int, y : int}"
    "choose({ x = zero | { y = one | {} } }, {x = one, y = zero})" |> success "{x : int, y : int}"
    "{{} - x}" |> failure "cannot unify types {x : _1 | _0} and {}"
    "{{x = one, y = zero} - x}" |> success "{y : int}"
    "{ x = true | {x = one}}" |> success "{x : bool, x : int}"
    "let a = {} in {b = one | a}" |> success "{b : int}"
    "let a = {x = one} in {x = true | a}.x" |> success "bool"
    "let a = {x = one} in a.y"|> failure "row does not contain label y"
    "let a = {x = one} in {a - x}" |> success "{}"
    "let a = {x = one} in let b = {x = true | a} in {b - x}.x" |> success "int"
    "fun r -> {x = one | r}" |> success "forall[r] {r} -> {x : int | r}"
    "fun r -> r.x" |> success "forall[r a] {x : a | r} -> a"
    "let get_x = fun r -> r.x in get_x({y = one, x = zero})" |> success "int"
    "let get_x = fun r -> r.x in get_x({y = one, z = true})" |> failure "row does not contain label x"
    "fun r -> choose({x = zero | r}, {x = one | {}})" |> success "{} -> {x : int}"
    "fun r -> choose({x = zero | r}, {x = one})" |> success "{} -> {x : int}"
    "fun r -> choose({x = zero | r}, {x = one | r})" |> success "forall[r] {r} -> {x : int | r}"
    "fun r -> choose({x = zero | r}, {y = one | r})" |> failure "recursive row types"
    "let f = fun x -> x.t(one) in f({t = succ})" |> success "int"
    "let f = fun x -> x.t(one) in f({t = id})" |> success "int"
    "{x = one, x = true}" |> success "{x : int, x : bool}"
    "let f = fun r -> let y = r.y in choose(r, {x = one, x = true}) in f" |> failure "row does not contain label y"
    "fun r -> let y = choose(r.x, one) in let z = choose({r - x}.x, true) in r" |> success "forall[a r] {x : int, x : bool | r} -> {x : int, x : bool | r}"
    "fun r -> choose({x = zero | r}, {x = one, x = true})" |> success "{x : bool} -> {x : int, x : bool}"
    "fun r -> choose(r, {x = one, x = true})" |> success "{x : int, x : bool} -> {x : int, x : bool}"
    "fun r -> choose({x = zero | r}, {x = true | r})" |> failure "cannot unify types int and bool"


[<EntryPoint>]
let main args = Program.main args
