module Core

let core =
    let add name tyString env =
        match Parser.parseType tyString with
        | Ok ty -> Infer.Env.extend env name ty
        | Error msg -> failwith msg

    Infer.Env.empty
    |> add "head" "forall[a] list[a] -> a"
    |> add "tail" "forall[a] list[a] -> list[a]"
    |> add "nil" "forall[a] list[a]"
    |> add "cons" "forall[a] (a, list[a]) -> list[a]"
    |> add "cons_curry" "forall[a] a -> list[a] -> list[a]"
    |> add "map" "forall[a b] (a -> b, list[a]) -> list[b]"
    |> add "map_curry" "forall[a b] (a -> b) -> list[a] -> list[b]"
    |> add "one" "int"
    |> add "zero" "int"
    |> add "succ" "int -> int"
    |> add "plus" "(int, int) -> int"
    |> add "eq" "forall[a] (a, a) -> bool"
    |> add "eq_curry" "forall[a] a -> a -> bool"
    |> add "not" "bool -> bool"
    |> add "true" "bool"
    |> add "false" "bool"
    |> add "pair" "forall[a b] (a, b) -> pair[a, b]"
    |> add "pair_curry" "forall[a b] a -> b -> pair[a, b]"
    |> add "first" "forall[a b] pair[a, b] -> a"
    |> add "second" "forall[a b] pair[a, b] -> b"
    |> add "id" "forall[a] a -> a"
    |> add "const" "forall[a b] a -> b -> a"
    |> add "apply" "forall[a b] (a -> b, a) -> b"
    |> add "apply_curry" "forall[a b] (a -> b) -> a -> b"
    |> add "choose" "forall[a] (a, a) -> a"
    |> add "choose_curry" "forall[a] a -> a -> a"
