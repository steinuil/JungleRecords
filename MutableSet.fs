module MutableSet


open System.Collections.Generic


type T<'item> = HashSet<'item>


let empty (): HashSet<'t> = new HashSet<'t>()


let add item (set: HashSet<'t>) = set.Add(item) |> ignore
