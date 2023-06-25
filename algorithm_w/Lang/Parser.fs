module Parser

open FParsec
open Microsoft.FSharp.Core.Option

open Expr

type UserState = unit // doesn't have to be unit, of course

let keyWordSet =
    System.Collections.Generic.HashSet<_>([| "fun"; "let"; "in"; "forall" |])

type Parser'<'t> = Parser<'t, UserState>

let ws: Parser'<unit> = CharParsers.spaces

let pchar_ws c : Parser'<char> = pchar c .>> ws

let pstring_ws s : Parser'<string> = pstring s .>> ws

let expr, exprRef = createParserForwardedToRef<Expr, unit> ()

let lowerIdentifier: Parser'<string> =
    let lowerIdentifierString: Parser<string, unit> =
        satisfy isLower .>>. manyChars (satisfy (fun c -> isLetter c || isDigit c || c = '_'))
        .>> ws
        |>> (fun (c, cs) -> (string c) + cs)

    let expectedIdentifier = expected "identifier"

    fun stream ->
        let state = stream.State
        let reply = lowerIdentifierString stream

        if reply.Status <> Ok || not (keyWordSet.Contains(reply.Result)) then
            reply
        else // result is keyword, so backtrack to before the string
            stream.BacktrackTo(state)
            Reply(Error, expectedIdentifier)

let factor: Parser'<Expr> =
    (lowerIdentifier |>> Var) <|> (between (pchar_ws '(') (pchar_ws ')') expr)

let param: Parser'<List<Expr>> =
    (between (pchar_ws '(') (pchar_ws ')') (sepBy expr (pchar_ws ',')))
    <|> (lowerIdentifier |>> (fun name -> [ Var name ]))

let apply: Parser'<Expr> =
    pipe2 factor (many param) (fun fn arg_list -> List.fold (fun f a -> Call(f, a)) fn arg_list)

exprRef.Value <-
    apply
    <|> (pstring_ws "let" >>. lowerIdentifier .>> pchar_ws '=' .>>. expr
         .>> pstring_ws "in"
         .>>. expr
         |>> fun ((n, e1), e2) -> Let(n, e1, e2))
    <|> (pstring_ws "fun" >>. many1 lowerIdentifier .>> pstring_ws "->" .>>. expr
         |>> fun (param_list, body_expr) -> Fun(param_list, body_expr))

let replace_ty_constants_with_vars var_name_list ty =
    let env =
        List.fold
            (fun env var_name -> Infer.Env.extend env var_name (Infer.new_gen_var ()))
            Infer.Env.empty
            var_name_list

    let rec f ty =
        match ty with
        | TConst name ->
            try
                Infer.Env.lookup env name
            with :? System.Collections.Generic.KeyNotFoundException ->
                ty
        | TVar _ -> ty
        | TApp(ty, ty_arg_list) -> TApp(f ty, List.map f ty_arg_list)
        | TArrow(param_ty_list, return_ty) -> TArrow(List.map f param_ty_list, f return_ty) in

    f ty


let ty, tyRef = createParserForwardedToRef<Ty, unit> ()

let simpleTypeTail2: Parser'<Ty -> Ty> =
    (pchar_ws ')' >>% (fun ty -> ty))
    <|> (pchar_ws ',' >>. sepBy1 ty (pchar_ws ',') .>> pchar_ws ')' .>> pstring_ws "->"
         .>>. ty
         |>> fun (param_ty_list, return_ty) -> fun ty -> TArrow([ ty ] @ param_ty_list, return_ty))

let simpleTypeTail1: Parser'<Ty> =
    ((pchar_ws ')' >>. pstring_ws "->" >>. ty)
     |>> fun return_ty -> TArrow([], return_ty))
    <|> ((ty .>>. simpleTypeTail2) |>> fun (ty, f) -> f ty)

let simpleType: Parser'<Ty> =
    ((pchar_ws '(') >>. simpleTypeTail1)
    <|> ((lowerIdentifier
          .>>. opt (
              pchar_ws '[' >>. sepBy1 ty (pchar_ws ',') .>> pchar_ws ']'
              |>> fun ty_arg_list -> fun ty -> TApp(ty, ty_arg_list)
          ))
         |>> fun (name, ty_arg_list_opt) ->
             match ty_arg_list_opt with
             | None -> TConst name
             | Some f -> f (TConst name))

tyRef.Value <-
    simpleType .>>. many (pstring_ws "->" >>. simpleType)
    |>> (fun (t, ts) -> [ t ] @ ts |> List.reduceBack (fun f g -> TArrow([ f ], g)))

let forallType: Parser'<Ty> =
    (pstring_ws "forall" >>. pchar_ws '[' >>. many1 lowerIdentifier .>> pchar_ws ']'
     .>>. ty
     |>> fun (var_name_list, ty) -> replace_ty_constants_with_vars var_name_list ty)
    <|> ty

let parseProduction p text =
    match run p text with
    | Success(result, _, l) -> Core.Ok result
    | Failure(msg, _, _) -> Core.Error msg

let parseExpr text = parseProduction (expr .>> eof) text

let parseType text =
    parseProduction (forallType .>> eof) text
