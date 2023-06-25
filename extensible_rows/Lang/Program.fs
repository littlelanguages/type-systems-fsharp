module Program

let private typeOf input =
    match Parser.parseExpr input with
    | Error msg -> msg
    | Ok expr ->
        try
            Infer.reset_id ()
            let ty = Infer.infer Core.core 0 expr
            let generalizedTy = Infer.generalize (-1) ty
            generalizedTy |> Expr.string_of_ty
        with ex ->
            ex.Message

[<EntryPoint>]
let main args =
    Seq.toList args |> List.iter (fun arg -> printfn "%s: %s" arg (typeOf arg))
    0
