module ParserTests

open Xunit
open Microsoft.FSharp.Core.Result
open Expr

[<Fact>]
let ``parse lowerIdentifier`` () =
    Assert.Equal("name" |> Core.Ok, Parser.parseProduction Parser.lowerIdentifier "name")
    Assert.Equal("nAm22e" |> Core.Ok, Parser.parseProduction Parser.lowerIdentifier "nAm22e")
    Assert.True(Parser.parseProduction Parser.lowerIdentifier "Name" |> Core.Result.isError)

let private success expected input =
    let expected' = expected |> Core.Ok
    let actual = Parser.parseExpr input

    if (expected' <> actual) then
        printfn "expected: %A" expected'
        printfn "actual: %A" actual

    Assert.Equal(expected', actual)

let private failure input =
    let parseResult = Parser.parseExpr input

    if (parseResult |> isOk) then
        printfn "expected error, but got: %A" parseResult

    Assert.True(parseResult |> isError)

[<Fact>]
let parseExpr () =
    "" |> failure
    "a" |> success (Var "a")
    "(a)" |> success (Var "a")
    "f(x, y)" |> success (Call(Var "f", [ Var "x"; Var "y" ]))

    "f(x)(y)" |> success (Call(Call(Var "f", [ Var "x" ]), [ Var "y" ]))
    "fun x -> x" |> success (Fun([ "x" ], Var "x"))

    "let f = fun x y -> g(x, y) in f(a, b)"
    |> success (Let("f", Fun([ "x"; "y" ], Call(Var "g", [ Var "x"; Var "y" ])), Call(Var "f", [ Var "a"; Var "b" ])))

    "let x = a in let y = b in f(x, y)"
    |> success (Let("x", Var "a", Let("y", Var "b", Call(Var "f", [ Var "x"; Var "y" ]))))

    "f x" |> success (Call(Var "f", [ Var "x" ]))

    "let a = one" |> failure
    "a, b" |> failure
    "a = b" |> failure
    "()" |> failure
    "fun x, y -> y" |> failure

    "{}" |> success RecordEmpty
    "{ }" |> success RecordEmpty
    "{" |> failure
    
    // (RecordSelect(Var "a", "x")) |> success "a.x"
    // "{m - a}" |> success (RecordRestrict(Var "m", "a"))
    "{m - a" |> failure
    "m - a" |> failure
    // "{a = x}" |> success (RecordExtend("a", Var "x", RecordEmpty))
    "{a = x" |> failure
    // "{a=x, b = y}" |> success (RecordExtend("a", Var "x", RecordExtend("b", Var "y", RecordEmpty)))
    // "{b = y ,a=x}" |> success (RecordExtend("b", Var "y", RecordExtend("a", Var "x", RecordEmpty)))
    // "{a=x,h=w,d=y,b=q,g=z,c=t,e=s,f=r}" |> success (RecordExtend("a", Var "x", RecordExtend("h", Var "w", RecordExtend("d", Var "y", RecordExtend("b", Var "q", RecordExtend("g", Var "z", RecordExtend("c", Var "t", RecordExtend("e", Var "s", RecordExtend("f", Var "r", RecordEmpty)))))))))
    // "{a = x|m}" |> success (RecordExtend ("a", Var "x", Var "m"))
    "{a | m}" |> failure
    // "{ a = x, b = y | m}" |> success (RecordExtend("a", Var "x", RecordExtend("b", Var "y", Var "m")))
    // "{ a = x, b = y | {m - a} }" |> success (RecordExtend("a", Var "x", RecordExtend("b", Var "y", RecordRestrict(Var "m", "a"))))
    "{ b = y | m - a }" |> failure
// "let x = {a = f(x), b = y.b} in { a = fun z -> z | {x - a} }" |> success (Let("x", RecordExtend("a", Call(Var "f", [Var "x"]), RecordExtend("b", RecordSelect(Var "y", "b"), RecordEmpty)), RecordExtend("a", Fun(["z"], Var "z"), RecordRestrict(Var "x", "a"))))
