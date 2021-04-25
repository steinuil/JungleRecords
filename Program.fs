module Deeper.Main


open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Audio
open System


let bgCount = 3
let cornerCount = 18
let frontCount = 18
let wallCount = 18

let recordsCount = 5

let mapCount = 3

let bgChangeHz = 40


type LoopDirection = Fwd | Bwd


type DeeperGame() as this =
    inherit Game()

    let graphics =
        new GraphicsDeviceManager(this, PreferredBackBufferWidth = 1024, PreferredBackBufferHeight = 768)

    // do this.IsMouseVisible <- true

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let backgrounds = Array.create bgCount Unchecked.defaultof<Texture2D>
    let corners = Array.create cornerCount Unchecked.defaultof<Texture2D>
    let fronts = Array.create frontCount Unchecked.defaultof<Texture2D>
    let walls = Array.create wallCount Unchecked.defaultof<Texture2D>
    let records = Array.create recordsCount Unchecked.defaultof<Texture2D>
    let mutable mapBg = Unchecked.defaultof<Texture2D>
    let mutable mapCursor = Unchecked.defaultof<Texture2D>
    let mutable mapWall = Unchecked.defaultof<Texture2D>

    let mutable stepSfx = Unchecked.defaultof<SoundEffect>
    let mutable stepShakySfx = Unchecked.defaultof<SoundEffect>
    let mutable stompSfx = Unchecked.defaultof<SoundEffect>
    let mutable foundSfx = Unchecked.defaultof<SoundEffect>

    let mutable bgHumAudioFile = Unchecked.defaultof<SoundEffect>
    let mutable bgHumAudio = Unchecked.defaultof<SoundEffectInstance>
    let mutable isBgHumPlaying = false

    let maps = Array.create mapCount Unchecked.defaultof<Maps.Map>

    let mutable bgIndex = 0

    let mutable currentMap = 0

    let mutable currentX = 0
    let mutable currentY = 0
    let mutable currentDirection = Maps.North
    let mutable currentRecord = None

    let mutable keysJustPressed = Set.empty
    let mutable keysJustReleased = Set.empty
    let mutable keysPressed = Set.empty


    let drawTexture tex =
        spriteBatch.Draw(tex, Rectangle(0, 0, 1024, 768), Color.White)


    // Minimap
    let drawMinimap map (x, y) =
        let drawMapTile tex x y =
            spriteBatch.Draw(tex, Rectangle(832 + (x * 15), 55 + (y * 15), 15, 15), Color.White)

        drawTexture mapBg

        Maps.minimap map (x, y)
        |> Array2D.iteri (fun y x ->
            function
            | Maps.MinimapTile.Floor -> drawMapTile mapWall x y
            | _ -> ())

        spriteBatch.Draw(
            mapCursor,
            Vector2(832.f + (4.f * 15.f) + 7.5f, 55.f + (4.f * 15.f) + 7.5f),
            Nullable<Rectangle>(),
            Color.White,
            Maps.directionToRadians(currentDirection),
            Vector2(7.5f, 7.5f),
            Vector2(1.f, 1.f),
            SpriteEffects.None,
            1.f
        )


    // Records
    let mutable recordIndex = 0
    let mutable recordLoopDirection = Fwd
    let mutable recordChangeFrequency = 10

    let updateRecord () =
        recordChangeFrequency <- recordChangeFrequency - 1
        if recordChangeFrequency = 0 then
            match recordLoopDirection with
            | Fwd when recordIndex = recordsCount - 1 ->
                recordLoopDirection <- Bwd
                recordIndex <- recordIndex - 1
            | Fwd ->
                recordIndex <- recordIndex + 1
            | Bwd when recordIndex = 0 ->
                recordLoopDirection <- Fwd
                recordIndex <- recordIndex + 1
            | Bwd ->
                recordIndex <- recordIndex - 1
            recordChangeFrequency <- 10

    let drawRecord pos =
        let draw (x, y) s =
            spriteBatch.Draw(records.[recordIndex], Rectangle(x, y, s, s), Color.White)

        match pos with
        | 12 ->
            draw (80, 284) 300
        | 22 ->
            draw (363, 284) 300
        | 32 ->
            draw (646, 284) 300

        | 1 ->
            draw (160, 331) 150
        | 11 ->
            draw (297, 331) 150
        | 21 ->
            draw (437, 331) 150
        | 31 ->
            draw (577, 331) 150
        | 41 ->
            draw (714, 331) 150

        | 0 ->
            draw (326, 354) 75
        | 10 ->
            draw (400, 354) 75
        | 20 ->
            draw (474, 354) 75
        | 30 ->
            draw (548, 354) 75
        | 40 ->
            draw (622, 354) 75

        | _ -> ()


    // Keys
    let updateKeys () =
        let pressed = Keyboard.GetState().GetPressedKeys() |> Set.ofArray

        keysJustPressed <- Set.difference pressed keysPressed
        keysJustReleased <- Set.difference keysPressed pressed
        keysPressed <- pressed


    override _.Initialize() =
        graphics.PreferredBackBufferWidth <- 1024
        graphics.PreferredBackBufferHeight <- 768
        graphics.ApplyChanges()

        this.Window.Title <- "Waiting for Jungle Records to Come in the Mail"

        base.Initialize()


    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)

        for i in 1 .. bgCount do
            backgrounds.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content\Textures\black %d" i)
        
        for i in 1 .. cornerCount do
            corners.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content\Textures\corner %d" i)

        for i in 1 .. frontCount do
            fronts.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content\Textures\front %d" i)

        for i in 1 .. wallCount do
            walls.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content\Textures\wall %d" i)

        for i in 1 .. recordsCount do
            records.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content\Textures\record %d" i)

        for i in 0 .. mapCount - 1 do
            maps.[i] <- Maps.loadMap (sprintf @"Content\Maps\level %d.txt" i)

        mapBg <- this.Content.Load<Texture2D>(@"Content\Textures\map bg")
        mapCursor <- this.Content.Load<Texture2D>(@"Content\Textures\map cursor")
        mapWall <- this.Content.Load<Texture2D>(@"Content\Textures\map wall")

        stepSfx <- this.Content.Load<SoundEffect>(@"Content\Sounds\step")
        stepShakySfx <- this.Content.Load<SoundEffect>(@"Content\Sounds\step shaky")
        stompSfx <- this.Content.Load<SoundEffect>(@"Content\Sounds\stomp")
        foundSfx <- this.Content.Load<SoundEffect>(@"Content\Sounds\found sfx")

        bgHumAudioFile <- this.Content.Load<SoundEffect>(@"Content\Sounds\bg hum")
        bgHumAudio <- bgHumAudioFile.CreateInstance()
        bgHumAudio.IsLooped <- true
        bgHumAudio.Volume <- 0.7f

        currentX <- maps.[currentMap].StartPosition.X
        currentY <- maps.[currentMap].StartPosition.Y
        currentDirection <- maps.[currentMap].StartDirection
        currentRecord <- maps.[currentMap].RecordPosition

        base.LoadContent()


    override _.Update(gameTime) =
        updateKeys ()

        if not isBgHumPlaying then
            bgHumAudio.Play()
            isBgHumPlaying <- true

        let mutable hasChanged = false

        let movement =
            if keysJustPressed.Contains Keys.W || keysJustPressed.Contains Keys.Up then
                Some Maps.Forwards
            elif keysJustPressed.Contains Keys.S || keysJustPressed.Contains Keys.Down then
                Some Maps.Backwards
            else
                None

        movement
        |> Option.bind (Maps.canMove maps.[currentMap].Tiles (currentX, currentY) currentDirection)
        |> Option.iter (fun (x, y) ->
            currentX <- x
            currentY <- y
            hasChanged <- true

            if maps.[currentMap].EndPosition = Point(currentX, currentY) then
                ignore <| stepShakySfx.Play()
            else
                ignore <| stepSfx.Play()

            match currentRecord with
            | Some pos when pos = Point(currentX, currentY) ->
                ignore <| foundSfx.Play()
                currentRecord <- None
            | _ -> ()
        )

        if Option.isSome movement then
            ()
        elif keysJustPressed.Contains Keys.D || keysJustPressed.Contains Keys.Right then
            currentDirection <- Maps.rotateDirection currentDirection Maps.Clockwise
            hasChanged <- true
        elif keysJustPressed.Contains Keys.A || keysJustPressed.Contains Keys.Left then
            currentDirection <- Maps.rotateDirection currentDirection Maps.Counterclockwise
            hasChanged <- true

        if
            not hasChanged &&
            keysJustPressed.Contains Keys.Space &&
            maps.[currentMap].EndPosition = Point(currentX, currentY)
        then
            currentMap <- (currentMap + 1) % mapCount
            currentX <- maps.[currentMap].StartPosition.X
            currentY <- maps.[currentMap].StartPosition.Y
            currentDirection <- maps.[currentMap].StartDirection
            currentRecord <- maps.[currentMap].RecordPosition
            hasChanged <- true
            stompSfx.Play() |> ignore

        if hasChanged then
            bgIndex <- (bgIndex + 1) % bgCount

        updateRecord ()

        base.Update(gameTime)


    override _.Draw(gameTime) =
        let bgColor = Color(160, 41, 160)

        let drawFront i = drawTexture fronts.[i - 1]
        let drawCorner i = drawTexture corners.[i - 1]
        let drawWall i = drawTexture walls.[i - 1]

        let drawPattern =
            Seq.iter (function
                | Patterns.Front i -> drawFront i
                | Patterns.Corner i -> drawCorner i
                | Patterns.Wall i -> drawWall i
                | Patterns.Record n -> drawRecord n)


        graphics.GraphicsDevice.Clear(bgColor)

        spriteBatch.Begin(blendState = BlendState.AlphaBlend, samplerState = SamplerState.PointClamp)
        do
            drawTexture backgrounds.[bgIndex]

            Patterns.translateMap maps.[currentMap].Tiles currentRecord (currentX, currentY) currentDirection
            |> Patterns.draw
            |> drawPattern

            drawMinimap maps.[currentMap].Tiles (currentX, currentY)

            // drawRecord()

        spriteBatch.End()

        base.Draw(gameTime)


[<EntryPoint; STAThread>]
let main _argv =
    use game = new DeeperGame()
    game.Run()
    0
