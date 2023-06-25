module ParserTests

open Xunit
open Microsoft.FSharp.Core.Result
open Expr

[<Fact>]
let ``parse lowerIdentifier`` () =
    Assert.Equal("name" |> Core.Ok, Parser.parseProduction Parser.lowerIdentifier "name")
    Assert.Equal("nAm22e" |> Core.Ok, Parser.parseProduction Parser.lowerIdentifier "nAm22e")
    Assert.True(Parser.parseProduction Parser.lowerIdentifier "Name" |> Core.Result.isError)

let success input expected =
    let expected' = expected |> Core.Ok
    let actual = Parser.parseExpr input

    if (expected' <> actual) then
        printfn "expected: %A" expected'
        printfn "actual: %A" actual

    Assert.Equal(expected', actual)

let failure input =
    let parseResult = Parser.parseExpr input

    if (parseResult |> isOk) then
        printfn "expected error, but got: %A" parseResult

    Assert.True(parseResult |> isError)

[<Fact>]
let parseExpr () =
    failure ""
    Var "a" |> success "a"
    Var "a" |> success "(a)"
    (Var "f", [ Var "x"; Var "y" ]) |> Call |> success "f(x, y)"
    (Call(Var "f", [ Var "x" ]), [ Var "y" ]) |> Call |> success "f(x)(y)"
    (Fun([ "x" ], Var "x")) |> success "fun x -> x"

    (Let("f", Fun([ "x"; "y" ], Call(Var "g", [ Var "x"; Var "y" ])), Call(Var "f", [ Var "a"; Var "b" ])))
    |> success "let f = fun x y -> g(x, y) in f(a, b)"

    (Let("x", Var "a", Let("y", Var "b", Call(Var "f", [ Var "x"; Var "y" ]))))
    |> success "let x = a in let y = b in f(x, y)"

    (Call(Var "f", [ Var "x" ])) |> success "f x"

    failure "let a = one"
    failure "a, b"
    failure "a = b"
    failure "()"
    failure "fun x, y -> y"
