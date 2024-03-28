module JungleRecords.Maps


open Microsoft.Xna.Framework


type CardinalDirection =
    | North
    | East
    | South
    | West


type RelativeDirection =
    | Forwards
    | Backwards
    | Left
    | Right


type Rotation =
    | Clockwise
    | Counterclockwise


type MapTile =
    | Wall
    | Floor


type Map = {
    Number: int
    Tiles: MapTile[,] // index with y,x
    StartPosition: Point
    EndPosition: Point
    StartDirection: CardinalDirection
    RecordPosition: Point option
    Triggers: MutableDict.T<int64, char>
}


let hashPosition x y =
    0L ||| int64 y ||| (int64 x <<< 32)


let loadMap n f =
    let lines =
        System.IO.File.ReadAllText(f, System.Text.Encoding.UTF8)
            .Split([|"\r\n"; "\n"|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> line.ToCharArray())
        |> array2D

    let mutable startPosition = Unchecked.defaultof<Point>
    let mutable endPosition = Unchecked.defaultof<Point>
    let mutable startDirection = Unchecked.defaultof<CardinalDirection>
    let mutable recordPosition = None

    let triggers = MutableDict.empty ()

    let tiles = lines |> Array2D.mapi (fun y x ->
        function
        | 'X' -> Wall
        | 'F' ->
            endPosition <- Point(x, y)
            Floor
        | ' ' -> Floor
        | 'N' ->
            startPosition <- Point(x, y)
            startDirection <- North
            Floor
        | 'E' ->
            startPosition <- Point(x, y)
            startDirection <- East
            Floor
        | 'S' ->
            startPosition <- Point(x, y)
            startDirection <- South
            Floor
        | 'W' ->
            startPosition <- Point(x, y)
            startDirection <- South
            Floor
        | 'R' ->
            recordPosition <- Some (Point(x, y))
            Floor
        | 'b' ->
            endPosition <- Point(x, y)
            triggers.[hashPosition x y] <- 'b'
            Floor
        | c when c >= 'a' && c <= 'z' ->
            triggers.[hashPosition x y] <- c
            Floor
        | c -> failwithf "unrecognized char: '%c'" c
    )

    {
        Number = n
        Tiles = tiles
        StartPosition = startPosition
        EndPosition = endPosition
        StartDirection = startDirection
        RecordPosition = recordPosition
        Triggers = triggers
    }


let rotate90 (tiles: MapTile[,]) =
    let height = Array2D.length1 tiles
    let width = Array2D.length2 tiles
    Array2D.init width height (fun row column ->
        tiles.[height - column - 1,row]
    )


let rotate tiles =
    function
    | North -> tiles
    | West -> rotate90 tiles
    | South -> (rotate90 >> rotate90) tiles
    | East -> (rotate90 >> rotate90 >> rotate90) tiles


let canMove tiles (x, y) currentDirection movementDirection =
    let dx, dy =
        match currentDirection, movementDirection with
        | North, Forwards
        | East, Left
        | South, Backwards
        | West, Right -> 0, -1

        | East, Forwards
        | South, Left
        | West, Backwards
        | North, Right -> 1, 0

        | South, Forwards
        | West, Left
        | North, Backwards
        | East, Right -> 0, 1

        | West, Forwards
        | North, Left
        | East, Backwards
        | South, Right -> -1, 0

    let tx = x + dx
    let ty = y + dy

    let mapHeight = Array2D.length1 tiles
    let mapWidth = Array2D.length2 tiles

    if tx >= 0 && tx < mapWidth && ty >= 0 && ty < mapHeight then
        if tiles.[ty, tx] = Floor then
            Some (tx, ty)
        else None
    else
        None


let rotateDirection dir rotation =
    match dir, rotation with
    | North, Clockwise -> East
    | North, Counterclockwise -> West
    | East, Clockwise -> South
    | East, Counterclockwise -> North
    | South, Clockwise -> West
    | South, Counterclockwise -> East
    | West, Clockwise -> North
    | West, Counterclockwise -> South


let directionToRadians =
    function
    | North -> 0.f
    | East -> MathHelper.ToRadians(90.f)
    | South -> MathHelper.ToRadians(180.f)
    | West -> MathHelper.ToRadians(270.f)


[<RequireQualifiedAccess>]
type MinimapTile = Wall | Floor | Empty


let minimap (map: MapTile[,]) (x, y) =
    let mapHeight = Array2D.length1 map
    let mapWidth = Array2D.length2 map

    let ox = x - 4
    let oy = y - 4

    Array2D.init 9 9 (fun y x ->
        let x = ox + x
        let y = oy + y

        if x >= 0 && x < mapWidth && y >= 0 && y < mapHeight then
            match map.[y, x] with
            | Floor -> MinimapTile.Floor
            | Wall -> MinimapTile.Wall
        else
            MinimapTile.Empty
    )


let levelToRecordMap =
    dict [
        1, 'u'
        2, 'v'
        3, 'w'
        4, 'z'
        5, 'x'
    ]
