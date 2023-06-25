module Parser

open FParsec
open Expr

type UserState = unit // doesn't have to be unit, of course

type Parser'<'t> = Parser<'t, UserState>

let private keyWordSet = Set.ofSeq [| "fun"; "let"; "in"; "forall" |]

let private ws: Parser'<unit> = CharParsers.spaces

let private pchar_ws c : Parser'<char> = pchar c .>> ws

let private pstring_ws s : Parser'<string> = pstring s .>> ws

let private expr, private exprRef = createParserForwardedToRef<Expr, unit> ()

let lowerIdentifier: Parser'<string> =
    let lowerIdentifierString: Parser<string, unit> =
        satisfy isLower
        .>>. manyChars (satisfy (fun c -> isLetter c || isDigit c || c = '_'))
        .>> ws
        |>> fun (c, cs) -> (string c) + cs

    let expectedIdentifier = expected "identifier"

    fun stream ->
        let state = stream.State
        let reply = lowerIdentifierString stream

        if reply.Status <> Ok || not (Set.contains reply.Result keyWordSet) then
            reply
        else
            stream.BacktrackTo state
            Reply(Error, expectedIdentifier)

let private factor: Parser'<Expr> =
    (lowerIdentifier |>> Var)
    <|> (between (pchar_ws '(') (pchar_ws ')') expr)
    <|> (pchar_ws '{' >>. expr .>> pchar_ws '-' .>>. lowerIdentifier .>> pchar_ws '}'
         |>> fun (e, n) -> RecordRestrict(e, n)
         |> attempt)
    <|> (pchar_ws '{'
         >>. sepBy (lowerIdentifier .>> pchar_ws '=' .>>. expr) (pchar_ws ',')
         .>>. opt (pchar_ws '|' >>. expr)
         .>> pchar_ws '}'
         |>> fun (fields, r) -> List.foldBack (fun (n, v) e -> RecordExtend(n, v, e)) fields (r |> Option.defaultValue RecordEmpty))

let private recordSelect: Parser'<Expr> =
    factor .>>. many ((pchar_ws '.') >>. lowerIdentifier)
    |>> fun (e, field_names) -> List.fold (fun e' n -> RecordSelect(e', n)) e field_names

let private param: Parser'<List<Expr>> =
    (between (pchar_ws '(') (pchar_ws ')') (sepBy expr (pchar_ws ',')))
    <|> (lowerIdentifier |>> fun name -> [ Var name ])

let private apply: Parser'<Expr> =
    recordSelect .>>. (many param)
    |>> fun (fn, arg_list) -> List.fold (fun f a -> Call(f, a)) fn arg_list

exprRef.Value <-
    apply
    <|> (pstring_ws "let" >>. lowerIdentifier .>> pchar_ws '=' .>>. expr
         .>> pstring_ws "in"
         .>>. expr
         |>> fun ((n, e1), e2) -> Let(n, e1, e2))
    <|> (pstring_ws "fun" >>. many1 lowerIdentifier .>> pstring_ws "->" .>>. expr
         |>> fun (param_list, body_expr) -> Fun(param_list, body_expr))

let private replace_ty_constants_with_vars var_name_list ty =
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

let private ty, private tyRef = createParserForwardedToRef<Ty, unit> ()

let private simpleTypeTail2: Parser'<Ty -> Ty> =
    (pchar_ws ')' >>% fun ty -> ty)
    <|> (pchar_ws ',' >>. sepBy1 ty (pchar_ws ',') .>> pchar_ws ')' .>> pstring_ws "->"
         .>>. ty
         |>> fun (param_ty_list, return_ty) -> fun ty -> TArrow([ ty ] @ param_ty_list, return_ty))

let private simpleTypeTail1: Parser'<Ty> =
    (pchar_ws ')' >>. pstring_ws "->" >>. ty
     |>> fun return_ty -> TArrow([], return_ty))
    <|> ((ty .>>. simpleTypeTail2) |>> fun (ty, f) -> f ty)

let private simpleType: Parser'<Ty> =
    (pchar_ws '(' >>. simpleTypeTail1)
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

let private forallType: Parser'<Ty> =
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
