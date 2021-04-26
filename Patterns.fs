module JungleRecords.Patterns


open Microsoft.Xna.Framework


type Item = RecordItem


type Tile = X | O of Item option


type Wireframe =
    | Front of int
    | Corner of int
    | Wall of int
    | Record of int


let translatePoint dir (width, height) (x, y) =
    match dir with
    | Maps.North ->
        x, y
    | Maps.East ->
        y,
        width - x - 1
    | Maps.South ->
        width - x - 1,
        height - y - 1
    | Maps.West ->
        height - y - 1,
        x

let translateMap (map: Maps.MapTile[,]) (record: Point option) (cx, cy) dir =
    let mapHeight = Array2D.length1 map
    let mapWidth = Array2D.length2 map

    let rx, ry = translatePoint dir (mapWidth, mapHeight) (cx, cy)

    let recordP = record |> Option.map (fun p -> translatePoint dir (mapWidth, mapHeight) (p.X, p.Y))

    let map = Maps.rotate map dir

    let mapHeight = Array2D.length1 map
    let mapWidth = Array2D.length2 map

    Array2D.init 4 5 (fun y x ->
        let ty = ry - 4 + 1 + y
        let tx = rx + 2 - 5 + 1 + x

        if tx >= 0 && tx < mapWidth && ty >= 0 && ty < mapHeight then
            match map.[ty, tx] with
            | Maps.Floor ->
                match recordP with
                | Some (px, py) ->
                    if px = tx && py = ty then
                        O (Some RecordItem)
                    else
                        O None
                | None -> O None
            | Maps.Wall -> X
        else
            X
    )


let wallRightOf =
    function
    | 0, 0 -> Some 15
    | 0, 1 -> Some 11
    | 0, 2 -> Some 9
    | 0, 3 -> None

    | 1, 0 -> Some 7
    | 1, 1 -> Some 5
    | 1, 2 -> Some 3
    | 1, 3 -> Some 1

    | 2, 0 -> Some 8
    | 2, 1 -> Some 6
    | 2, 2 -> Some 4
    | 2, 3 -> Some 2

    | 3, 0 -> Some 16
    | 3, 1 -> Some 12
    | 3, 2 -> Some 10
    | 3, 3 -> None

    | x, y -> failwithf "invalid coordinate: %d,%d" x y


let frontBelow =
    function
    | 0, 0 -> Some 14
    | 0, 1 -> Some 13
    | 0, 2 -> None

    | 1, 0 -> Some 7
    | 1, 1 -> Some 6
    | 1, 2 -> Some 5

    | 2, 0 -> Some 3
    | 2, 1 -> Some 2
    | 2, 2 -> Some 1

    | 3, 0 -> Some 11
    | 3, 1 -> Some 10
    | 3, 2 -> Some 9

    | 4, 0 -> Some 17
    | 4, 1 -> Some 16
    | 4, 2 -> None

    | x, y -> failwithf "invalid coordinate: %d,%d" x y


let cornerBottomRightOf =
    function
    | 0, 0 -> Some 9
    | 0, 1 -> Some 7
    | 0, 2 -> None

    | 1, 0 -> Some 5
    | 1, 1 -> Some 3
    | 1, 2 -> Some 1

    | 2, 0 -> Some 6
    | 2, 1 -> Some 4
    | 2, 2 -> Some 2

    | 3, 0 -> Some 10
    | 3, 1 -> Some 8
    | 3, 2 -> None

    | x, y -> failwithf "invalid coordinate: %d,%d" x y


let recordOn x y = x * 10 + y


let draw map =
    let height = Array2D.length1 map
    let width = Array2D.length2 map

    seq {
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                if x < width - 1 then
                    match map.[y, x], map.[y, x + 1] with
                    | X, X   | O _, O _ -> ()
                    | X, O _ | O _, X    ->
                        match wallRightOf (x, y) with
                        | Some n -> yield Wall n
                        | _ -> ()

                match map.[y, x] with
                | O (Some RecordItem) ->
                    yield Record (recordOn x y)
                | _ -> ()

            for x in 0 .. width - 1 do
                if y < height - 1 && x < width - 1 then
                    match map.[y, x], map.[y, x + 1],
                          map.[y + 1, x], map.[y + 1, x + 1] with
                    | X,   X,
                      X,   O _
                    | X,   X,
                      O _, X

                    | O _, X,
                      X,   X
                    | X,   O _,
                      X,   X

                    | X,   O _,
                      O _, O _ 
                    | O _, X,
                      O _, O _

                    | O _, O _,
                      X,   O _
                    | O _, O _,
                      O _, X

                    | X,   O _,
                      O _, X
                    | O _, X,
                      X,   O _ ->
                        match cornerBottomRightOf (x, y) with
                        | Some n -> yield Corner n
                        | _ -> ()

                    | _ -> ()

                if y < height - 1 then
                    match map.[y, x], map.[y + 1, x] with
                    | X, X   | O _, O _ -> ()
                    | X, O _ | O _, X   ->
                        match frontBelow (x, y) with
                        | Some n -> yield Front n
                        | _ -> ()
    }
