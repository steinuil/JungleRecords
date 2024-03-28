module JungleRecords.Main


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

let levelIndicatorBgCount = 2
let levelNumberCount = 7

let endCardCount = 2

let mapCount = 6

let hz = 5


type LoopDirection = Fwd | Bwd


type GameState =
    | Start of int
    | Text of id:char
    | Dungeon
    | Battle of recordId:char
    | End of int


type JungleRecordsGame() as this =
    inherit Game()

    let graphics =
        new GraphicsDeviceManager(this, PreferredBackBufferWidth = 1024, PreferredBackBufferHeight = 768)

    do this.IsMouseVisible <- true

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let backgrounds = Array.create bgCount Unchecked.defaultof<Texture2D>
    let corners = Array.create cornerCount Unchecked.defaultof<Texture2D>
    let fronts = Array.create frontCount Unchecked.defaultof<Texture2D>
    let walls = Array.create wallCount Unchecked.defaultof<Texture2D>
    let records = Array.create recordsCount Unchecked.defaultof<Texture2D>
    let mutable mapBg = Unchecked.defaultof<Texture2D>
    let mutable mapCursor = Unchecked.defaultof<Texture2D>
    let mutable mapWall = Unchecked.defaultof<Texture2D>

    let levelIndicatorBg = Array.create levelIndicatorBgCount Unchecked.defaultof<Texture2D>
    let mutable levelIndicatorText = Unchecked.defaultof<Texture2D>
    let levelNumbers = Array.create levelNumberCount Unchecked.defaultof<Texture2D>
    let mutable levelIndicatorBgChange = hz
    let mutable levelIndicatorBgIndex = 0

    let mutable titleCard1 = Unchecked.defaultof<Texture2D>
    let mutable titleCard2 = Unchecked.defaultof<Texture2D>
    let mutable titleJingle = Unchecked.defaultof<SoundEffect>

    let mutable stepSfx = Unchecked.defaultof<SoundEffect>
    let mutable stepShakySfx = Unchecked.defaultof<SoundEffect>
    let mutable stompSfx = Unchecked.defaultof<SoundEffect>
    let mutable foundSfx = Unchecked.defaultof<SoundEffect>

    let mutable bgHumAudioFile = Unchecked.defaultof<SoundEffect>
    let mutable bgHumAudio = Unchecked.defaultof<SoundEffectInstance>

    let mutable endingBgmFile = Unchecked.defaultof<SoundEffect>
    let mutable endingBgm = Unchecked.defaultof<SoundEffectInstance>

    let maps = Array.create mapCount Unchecked.defaultof<Maps.Map>

    let mutable endCards = Array.create endCardCount Unchecked.defaultof<Texture2D>

    let mutable bgIndex = 0
    let mutable bgChange = hz

    let mutable currentMap = 0

    let mutable currentX = 0
    let mutable currentY = 0
    let mutable currentDirection = Maps.North
    let mutable currentRecord = None

    let mutable keysJustPressed = Set.empty
    let mutable keysJustReleased = Set.empty
    let mutable keysPressed = Set.empty


    let mutable gameState = Start 360

    let clearedDialogues = MutableSet.empty ()


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
        let pressed =
            Keyboard.GetState().GetPressedKeys() |> Set.ofArray
            |> Set.filter (
                function
                | Keys.A
                | Keys.B
                | Keys.C
                | Keys.D
                | Keys.E
                | Keys.F
                | Keys.G
                | Keys.H
                | Keys.I
                | Keys.J
                | Keys.K
                | Keys.L
                | Keys.M
                | Keys.N
                | Keys.O
                | Keys.P
                | Keys.Q
                | Keys.R
                | Keys.S
                | Keys.T
                | Keys.U
                | Keys.V
                | Keys.W
                | Keys.X
                | Keys.Y
                | Keys.Z
                | Keys.Up | Keys.Down | Keys.Left | Keys.Right
                | Keys.Space
                | Keys.Delete
                | Keys.Enter
                | Keys.Escape -> true
                | _ -> false
            )

        keysJustPressed <- Set.difference pressed keysPressed
        keysJustReleased <- Set.difference keysPressed pressed
        keysPressed <- pressed


    // Level number indicator
    let updateLevelNumberIndicator () =
        if levelIndicatorBgChange = 0 then
            levelIndicatorBgChange <- hz
            levelIndicatorBgIndex <- (levelIndicatorBgIndex + 1) % levelIndicatorBgCount
        else
            levelIndicatorBgChange <- levelIndicatorBgChange - 1

    let drawLevelNumberIndicator () =
        drawTexture levelIndicatorBg.[levelIndicatorBgIndex]
        drawTexture levelIndicatorText

        spriteBatch.Draw(
            levelNumbers.[maps.[currentMap].Number],
            Rectangle(161, 26, 40, 40),
            Color.White
        )


    // Background
    let updateBackground () =
        if bgChange = 0 then
            bgChange <- hz
            bgIndex <- (bgIndex + 1) % bgCount
        else
            bgChange <- bgChange - 1


    // Systems
    let dialogueSystem = Dialogue.DialogueSystem(hz)
    let battleSystem = Battle.BattleSystem(dialogueSystem, hz)


    override _.Initialize() =
        graphics.PreferredBackBufferWidth <- 1024
        graphics.PreferredBackBufferHeight <- 768
        graphics.ApplyChanges()

        this.Window.Title <- "Waiting for Jungle Records to Come in the Mail"

        base.Initialize()


    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)

        let loadTexture tex =
            this.Content.Load<Texture2D>(@"Content/Textures/" + tex)

        let loadSfx sfx =
            this.Content.Load<SoundEffect>(@"Content/Sounds/" + sfx)

        for i in 1 .. bgCount do
            backgrounds.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content/Textures/black %d" i)
        
        for i in 1 .. cornerCount do
            corners.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content/Textures/corner %d" i)

        for i in 1 .. frontCount do
            fronts.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content/Textures/front %d" i)

        for i in 1 .. wallCount do
            walls.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content/Textures/wall %d" i)

        for i in 1 .. recordsCount do
            records.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content/Textures/record %d" i)

        for i in 0 .. mapCount - 1 do
            maps.[i] <- Maps.loadMap i (sprintf @"Content/Maps/level %d.txt" i)

        for i in 1 .. levelIndicatorBgCount do
            levelIndicatorBg.[i - 1] <- this.Content.Load<Texture2D>(sprintf @"Content/Textures/level bg %d" i)

        levelIndicatorText <- this.Content.Load<Texture2D>(@"Content/Textures/level writing")

        for i in 0 .. levelNumberCount - 1 do
            levelNumbers.[i] <- this.Content.Load<Texture2D>(sprintf @"Content/Textures/level %d" i)

        mapBg <- this.Content.Load<Texture2D>(@"Content/Textures/map bg")
        mapCursor <- this.Content.Load<Texture2D>(@"Content/Textures/map cursor")
        mapWall <- this.Content.Load<Texture2D>(@"Content/Textures/map wall")

        titleCard1 <- this.Content.Load<Texture2D>(@"Content/Textures/title")
        titleCard2 <- this.Content.Load<Texture2D>(@"Content/Textures/title 2")
        titleJingle <- this.Content.Load<SoundEffect>(@"Content/Sounds/title")

        stepSfx <- this.Content.Load<SoundEffect>(@"Content/Sounds/step")
        stepShakySfx <- this.Content.Load<SoundEffect>(@"Content/Sounds/step shaky")
        stompSfx <- this.Content.Load<SoundEffect>(@"Content/Sounds/stomp")
        foundSfx <- this.Content.Load<SoundEffect>(@"Content/Sounds/found sfx")

        bgHumAudioFile <- this.Content.Load<SoundEffect>(@"Content/Sounds/bg hum")
        bgHumAudio <- bgHumAudioFile.CreateInstance()
        bgHumAudio.IsLooped <- true
        bgHumAudio.Volume <- 0.7f

        currentX <- maps.[currentMap].StartPosition.X
        currentY <- maps.[currentMap].StartPosition.Y
        currentDirection <- maps.[currentMap].StartDirection
        currentRecord <- maps.[currentMap].RecordPosition

        for i in 1 .. endCardCount do
            endCards.[i - 1] <- loadTexture (sprintf "end %d" i)

        endingBgmFile <- loadSfx "ending"
        endingBgm <- endingBgmFile.CreateInstance()
        endingBgm.IsLooped <- true

        dialogueSystem.LoadContent(this.Content)
        battleSystem.LoadContent(this.Content)

        base.LoadContent()


    override _.Update(gameTime) =
        updateKeys ()

        let startDialogue id =
            gameState <- Text id
            dialogueSystem.SetDialogue(id)

        match gameState with
        | Start 0 ->
            startDialogue 'a'
            bgHumAudio.Play()
        
        | Start n ->
            if n = 360 then
                titleJingle.Play() |> ignore

            updateBackground ()

            gameState <- Start (n - 1)


        | Dungeon ->
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
                    currentRecord <- None
                    let currentLevel = maps.[currentMap].Number
                    battleSystem.StartBattle currentLevel
                    gameState <- Battle Maps.levelToRecordMap.[currentLevel]

                | _ -> ()

                match MutableDict.tryFind (Maps.hashPosition currentX currentY) maps.[currentMap].Triggers with
                | Some t when not (clearedDialogues.Contains t) -> startDialogue t
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
                (keysJustPressed.Contains Keys.Space || keysJustPressed.Contains Keys.E) &&
                maps.[currentMap].EndPosition = Point(currentX, currentY)
            then
                if Option.isSome currentRecord then
                    startDialogue 'd'
                elif currentMap >= mapCount - 1 then
                    gameState <- End 0
                    stompSfx.Play() |> ignore
                else
                    currentMap <- currentMap + 1
                    currentX <- maps.[currentMap].StartPosition.X
                    currentY <- maps.[currentMap].StartPosition.Y
                    currentDirection <- maps.[currentMap].StartDirection
                    currentRecord <- maps.[currentMap].RecordPosition
                    hasChanged <- true
                    stompSfx.Play() |> ignore


            if hasChanged then
                bgIndex <- (bgIndex + 1) % bgCount


        | Text id ->
            let wasAnyKeyPressed = not <| Set.isEmpty keysJustPressed

            let isOver = dialogueSystem.Update(wasAnyKeyPressed)
            if isOver then
                gameState <- Dungeon
                MutableSet.add id clearedDialogues


        | Battle recordId ->
            match battleSystem.Update(keysJustPressed) with
            | Battle.Continue -> ()
            | Battle.Won ->
                ignore <| foundSfx.Play()
                startDialogue recordId
            | Battle.Exit ->
                this.Exit()


        | End 0 ->
            if bgHumAudio.State = SoundState.Playing then
                bgHumAudio.Stop()
                endingBgm.Play()

            updateBackground ()

            let wasAnyKeyPressed = not <| Set.isEmpty keysJustPressed

            if wasAnyKeyPressed then
                gameState <- End 1


        | End _ ->
            if keysJustPressed.Contains Keys.Space || keysJustPressed.Contains Keys.E then
                this.Exit()

            updateBackground ()


        updateRecord ()
        updateLevelNumberIndicator ()

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

            match gameState with
            | Dungeon
            | Text _ ->
                Patterns.translateMap maps.[currentMap].Tiles currentRecord (currentX, currentY) currentDirection
                |> Patterns.draw
                |> drawPattern

                drawMinimap maps.[currentMap].Tiles (currentX, currentY)

                drawLevelNumberIndicator ()

                match gameState with
                | Text _ ->
                    dialogueSystem.Draw(spriteBatch)
                | _ -> ()
            

            | Start n ->
                if n <= 310 && n > 180 then
                    drawTexture titleCard1
                elif n <= 180 && n > 60 then
                    drawTexture titleCard2


            | Battle _ ->
                battleSystem.Draw(spriteBatch)


            | End n ->
                drawTexture endCards.[n]



        spriteBatch.End()

        base.Draw(gameTime)


[<EntryPoint; STAThread>]
let main _argv =
    use game = new JungleRecordsGame()
    game.Run()
    0
