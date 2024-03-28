module MutableDict


open System.Collections.Generic


type T<'k, 'v> = Dictionary<'k, 'v>


let empty (): Dictionary<'k, 'v> = new Dictionary<'k, 'v>()


let tryFind key (dict: Dictionary<_, _>) =
    match dict.TryGetValue key with
    | true, v -> Some v
    | false, _ -> None


let find key (dict: Dictionary<_, _>) = dict.[key]


let add key value (dict: Dictionary<_, _>) = dict.[key] <- value
