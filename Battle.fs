module JungleRecords.Battle


open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Audio


type EnemyAction =
    | Damage of int
    | Defend
    | Nothing
    | Suicide


type EnemySideEffect =
    | TookDamage of int
    | Blocked
    | Text of char


type PlayerAction =
    | Attack
    | Bark
    | Dodge


type Turn = MyTurn | EnemyTurn of PlayerAction


type Enemy =
    abstract Health: int
    abstract IsDefending: bool
    abstract IsRecovering: bool
    abstract IsStartled: bool
    abstract InitialMessage: char
    abstract NextAction: playerAction: PlayerAction -> EnemyAction * EnemySideEffect list


// Behavior: keeps attacking until you bark, then stops attacking.
// Strategy: bark and then attack until it dies.
type Enemy1() =
    let mutable isScared = false
    let mutable health = 20
    let mutable lastAction = Nothing

    interface Enemy with
        member _.Health = health

        member _.IsDefending = false

        member _.IsRecovering = false

        member _.IsStartled = isScared

        member _.InitialMessage = 'n'

        member _.NextAction playerAction =
            let enemyAction, sideEffects =
                match playerAction, lastAction with
                | Attack, _ when isScared ->
                    health <- health - 5
                    Nothing, [
                        TookDamage 5
                        Text 's'
                    ]
                | Bark, _ ->
                    isScared <- true
                    Nothing, [
                        Text 's'
                    ]
                | Attack, Defend ->
                    Damage 3, [
                        Blocked
                    ]
                | Attack, _ ->
                    health <- health - 1
                    Damage 3, [
                        TookDamage 1
                    ]
                | Dodge, _ when isScared ->
                    Nothing, [
                        Text 's'
                    ]
                | Dodge, _ ->
                    Damage 3, []

            lastAction <- enemyAction
            enemyAction, sideEffects


type Enemy2AttackState = E2Hit | E2Miss


// Behavior: flip-flips between hit and miss.
// Strategy: alternate between attack and dodge.
type Enemy2() =
    let mutable health = 20

    let mutable state = E2Hit

    interface Enemy with
        member _.Health = health

        member _.IsDefending = false

        member _.IsRecovering = false

        member _.IsStartled = false

        member _.InitialMessage = 'n'

        member _.NextAction playerAction =
            let enemyAction =
                match state with
                | E2Hit ->
                    state <- E2Miss
                    Damage 8
                | E2Miss ->
                    state <- E2Hit
                    Damage 0

            let sideEffects =
                match playerAction with 
                | Attack ->
                    health <- health - 3
                    [
                        TookDamage 3
                        Text 'n'
                    ]
                | _ ->
                    []

            enemyAction, sideEffects


// Behavior: does 3 consecutive hits and then recovers for 1 turn.
// If you bark during the recovery you get 3 turns of recovery.
// Strategy: dodge for 3 turns and then either attack, or bark and then attack 3 times.
type Enemy3State =
    | E3Attacking of int
    | E3Recovering
    | E3Startled of int


type Enemy3() =
    let mutable health = 20

    let mutable state = E3Attacking 3

    interface Enemy with
        member _.Health = health

        member _.IsDefending = false

        member _.IsRecovering =
            match state with E3Recovering -> true | _ -> false

        member _.IsStartled = 
            match state with E3Startled _ -> true | _ -> false

        member _.InitialMessage = 'n'

        member _.NextAction playerAction =
            let wasHit = playerAction = Attack

            match state with
            | E3Attacking 1 ->
                state <- E3Recovering
                if wasHit then health <- health - 3
                Damage 6, [
                    if wasHit then yield TookDamage 3
                    yield Text 't'
                ]

            | E3Attacking n ->
                state <- E3Attacking (n - 1)
                if wasHit then health <- health - 3
                Damage 6, [
                    if wasHit then yield TookDamage 3
                    yield Text 'n'
                ]

            | E3Recovering when playerAction = Bark ->
                state <- E3Startled 3
                Nothing, [ Text 's' ]

            | E3Recovering ->
                state <- E3Attacking 3
                if wasHit then health <- health - 3
                Nothing, [
                    if wasHit then yield TookDamage 3
                    yield Text 'n'
                ]

            | E3Startled 1 ->
                state <- E3Attacking 3
                if wasHit then health <- health - 4
                Nothing, [
                    if wasHit then yield TookDamage 4
                    yield Text 'n'
                ]

            | E3Startled n ->
                state <- E3Startled (n - 1)
                if wasHit then health <- health - 4
                Nothing, [
                    if wasHit then yield TookDamage 4
                    yield Text 's'
                ]


// Behavior: defends until bark, then hits, then 1 recovery
// Strategy: bark, defend, attack
type Enemy4State =
    | E4Defending
    | E4Attacking
    | E4Recovering


type Enemy4() =
    let mutable health = 20

    let mutable state = E4Defending

    interface Enemy with
        member _.Health = health

        member _.IsDefending = state = E4Defending

        member _.IsRecovering = state = E4Recovering

        member _.IsStartled = false

        member _.InitialMessage = '2'

        member _.NextAction playerAction =
            match state, playerAction with
            | E4Defending, Bark ->
                state <- E4Attacking
                Nothing, [ Text 'n' ]

            | E4Attacking, Attack ->
                state <- E4Recovering
                Damage 16, [
                    TookDamage 1
                    Text 't'
                ]

            | E4Attacking, Bark ->
                state <- E4Recovering
                Damage 16, [ Text 't' ]

            | E4Attacking, Dodge ->
                state <- E4Recovering
                Damage 8, [ Text 't' ]

            | E4Recovering, Attack ->
                health <- health - 5
                state <- E4Defending
                Nothing, [
                    TookDamage 5
                    Text '2'
                ]

            | E4Recovering, _ ->
                state <- E4Defending
                Nothing, [ Text '2' ]

            | E4Defending, Attack ->
                Nothing, [
                    Text 'q'
                    Text '2'
                ]

            | E4Defending, _ ->
                Nothing, [ Text '2' ]


// Behavior: invulnerable to attacks until you bark.
// When you bark it croaks back.
// If you get 5 consecutive croaks the monster becomes angry and attacks for 5 turns,
// then explodes.
// Strategy: bark, bark, bark, bark, bark, defend, defend, defend, defend, defend.
type Enemy5State =
    | E5Defending
    | E5Croaking of int
    | E5Attacking of int
    | E5Exploded


type Enemy5() =
    let mutable state = E5Defending

    interface Enemy with
        member _.Health = match state with E5Exploded -> 0 | _ -> 20

        member _.IsDefending = match state with E5Defending -> true | _ -> false

        member _.IsRecovering = false

        member _.IsStartled = false

        member _.InitialMessage = 'm'

        member _.NextAction playerAction =
            match state, playerAction with
            | E5Defending, Bark ->
                state <- E5Croaking 2
                Nothing, [
                    Text '3'
                ]

            | E5Croaking 1, Bark ->
                state <- E5Attacking 4
                Nothing, [
                    Text 'n'
                ]

            | E5Croaking n, Bark ->
                state <- E5Croaking (n - 1)
                Nothing, [
                    Text '3'
                    Text '9'
                ]

            | E5Croaking _, Dodge ->
                state <- E5Defending
                Nothing, [
                    Text '2'
                    Text 'm'
                ]

            | _, Attack ->
                state <- E5Defending
                Damage 10, [
                    Text '2'
                ]

            | E5Defending, _ ->
                Nothing, [
                    Text '2'
                    Text 'm'
                ]

            | E5Attacking 0, _ ->
                state <- E5Exploded
                Suicide, []

            | E5Attacking n, _ ->
                state <- E5Attacking (n - 1)
                Damage 10, [
                    Text 'n'
                ]

            | E5Exploded _, _ ->
                failwith "the monster is already dead"



let enemyByLevel: int -> Enemy =
    function
    | 1 -> Enemy1() :> Enemy
    | 2 -> Enemy2() :> Enemy
    | 3 -> Enemy3() :> Enemy
    | 4 -> Enemy4() :> Enemy
    | 5 -> Enemy5() :> Enemy
    | n -> failwithf "invalid: %d" n


type TurnOutcome =
    | EnemyDied
    | YouDied
    | NoOutcome


let enemiesCount = 5
let battleSelectorTextureCount = 2
let healthTextureCount = 2


let shakeHz = 2


type SystemState = BattleState | TextState of SystemState | DiedState | WonState


type UpdateResult = Continue | Won | Exit


type BattleSystem(dialogueSystem: Dialogue.DialogueSystem, hz: int) =
    let mutable currentEnemy = Unchecked.defaultof<Enemy>
    let mutable currentEnemyIndex = 0
    let mutable currentTurn = MyTurn
    // let mutable isShowingText = false
    let mutable state = BattleState

    let mutable selectedOptionIdx = 0

    let mutable yourHealth = 20

    let mutable actionQueue = []

    let popActionQueue () =
        match actionQueue with
        | [] -> None
        | item :: rest ->
            actionQueue <- rest
            Some item


    let enemiesTexture = Array.create enemiesCount Unchecked.defaultof<Texture2D>
    let mutable battleUiTexture = Unchecked.defaultof<Texture2D>
    let battleSelectorTexture = Array.create battleSelectorTextureCount Unchecked.defaultof<Texture2D>
    let healthTexture = Array.create healthTextureCount Unchecked.defaultof<Texture2D>

    let mutable battleSelectorChange = hz
    let mutable battleSelectorIndex = 0
    let updateBattleSelector () =
        if battleSelectorChange = 0 then
            battleSelectorIndex <- (battleSelectorIndex + 1) % battleSelectorTextureCount
            battleSelectorChange <- hz
        else
            battleSelectorChange <- battleSelectorChange - 1

    let mutable healthBarChange = hz
    let mutable healthBarIndex = 0
    let updateHealthBar () =
        if healthBarChange = 0 then
            healthBarIndex <- (healthBarIndex + 1) % healthTextureCount
            healthBarChange <- hz
        else
            healthBarChange <- healthBarChange - 1

    let mutable selectSfx = Unchecked.defaultof<SoundEffect>
    let mutable actionSfx = Unchecked.defaultof<SoundEffect>

    let mutable youDiedTexture = Unchecked.defaultof<Texture2D>

    let mutable hitSfx = Unchecked.defaultof<SoundEffect>


    let pushDialogue dialogueId =
        actionQueue <- Text dialogueId :: actionQueue


    let startDialogue dialogueId =
        match state with
        | TextState _ -> ()
        | prevState ->
            state <- TextState prevState
        dialogueSystem.SetDialogue(dialogueId)


    // let mutable battleOutcome = NoOutcome

    let mutable enemyShake = 0
    let mutable screenShake = 0

    let shakeScreen () =
        screenShake <- 20

    let shakeEnemy () =
        enemyShake <- 20

    let updateShake () =
        if enemyShake > 0 then
            enemyShake <- enemyShake - 1
        if screenShake > 0 then
            screenShake <- screenShake - 1


    let resetBattle level =
        currentEnemy <- enemyByLevel level
        currentEnemyIndex <- level - 1

        currentTurn <- MyTurn
        selectedOptionIdx <- 0
        yourHealth <- 20
        state <- BattleState
        selectedOptionIdx <- 0
        // battleOutcome <- NoOutcome

        pushDialogue currentEnemy.InitialMessage


    member _.LoadContent(content: ContentManager) =
        let loadTexture name =
            content.Load<Texture2D>(@"Content\Textures\" + name)

        let loadSfx name =
            content.Load<SoundEffect>(@"Content\Sounds\" + name)

        for i in 1 .. enemiesCount do
            enemiesTexture.[i - 1] <- loadTexture (sprintf "enemy %d" i)

        battleUiTexture <- loadTexture "battle ui"

        for i in 1 .. battleSelectorTextureCount do
            battleSelectorTexture.[i - 1] <- loadTexture (sprintf "battle selector %d" i)

        for i in 1 .. healthTextureCount do
            healthTexture.[i - 1] <- loadTexture (sprintf "health bar %d" i)

        selectSfx <- loadSfx "select"
        actionSfx <- loadSfx "action"

        youDiedTexture <- loadTexture "you died"

        hitSfx <- loadSfx "hit"


    member _.StartBattle level =
        resetBattle level
        pushDialogue '0'


    member _.Update(keysJustPressed: Set<Keys>) =
        updateBattleSelector ()
        updateHealthBar ()
        updateShake ()

        let updateFromActionQueue () =
            match popActionQueue () with
            | None ->
                match state with
                | TextState prevState ->
                    state <- prevState
                | _ -> ()
            | Some (TookDamage _) ->
                startDialogue 'r'
            | Some Blocked ->
                startDialogue 'q'
            | Some (Text dialogueId) ->
                startDialogue dialogueId

        match state with
        | TextState _ ->
            let isOver = dialogueSystem.Update(not (Set.isEmpty keysJustPressed))
            if isOver then updateFromActionQueue ()

            Continue

        | BattleState | WonState | DiedState when not (List.isEmpty actionQueue) ->
            updateFromActionQueue ()

            Continue
        
        | BattleState ->
            match currentTurn with
            | EnemyTurn playerAction ->
                let prevEnemyHealth = currentEnemy.Health

                let enemyAction, sideEffects = currentEnemy.NextAction playerAction

                if currentEnemy.Health <> prevEnemyHealth then
                    hitSfx.Play() |> ignore
                    shakeEnemy ()

                if currentEnemy.Health < 1 && enemyAction <> Suicide then
                    pushDialogue '+'
                    state <- WonState

                else
                    match enemyAction with
                    | Damage 0 ->
                        currentTurn <- MyTurn
                        actionQueue <- sideEffects
                        pushDialogue '1'
                    | Damage n ->
                        let dialogue =
                            if playerAction = Dodge then
                                'o'
                            else
                                yourHealth <- yourHealth - n
                                shakeScreen ()
                                if currentEnemy.Health = prevEnemyHealth then
                                    hitSfx.Play() |> ignore

                                'p'

                        if yourHealth < 1 then
                            pushDialogue '-'
                            state <- DiedState
                            selectedOptionIdx <- 0
                        else
                            currentTurn <- MyTurn
                            actionQueue <- sideEffects
                            pushDialogue dialogue
                    | Defend when playerAction = Attack ->
                        currentTurn <- MyTurn
                        actionQueue <- sideEffects
                        pushDialogue 'q'
                    
                    | Defend ->
                        currentTurn <- MyTurn
                        actionQueue <- sideEffects
                        pushDialogue '2'

                    | Nothing ->
                        currentTurn <- MyTurn
                        actionQueue <- sideEffects

                    | Suicide ->
                        pushDialogue '+'
                        pushDialogue '4'
                        state <- WonState

            | MyTurn ->
                if keysJustPressed.Contains Keys.W || keysJustPressed.Contains Keys.Up then
                    selectedOptionIdx <- ((selectedOptionIdx - 1) + 3) % 3
                    selectSfx.Play() |> ignore

                elif keysJustPressed.Contains Keys.S || keysJustPressed.Contains Keys.Down then
                    selectedOptionIdx <- (selectedOptionIdx + 1) % 3
                    selectSfx.Play() |> ignore

                elif keysJustPressed.Contains Keys.Space || keysJustPressed.Contains Keys.E then
                    actionSfx.Play () |> ignore

                    let playerAction =
                        match selectedOptionIdx with
                        | 0 -> Bark
                        | 1 -> Attack
                        | 2 -> Dodge
                        | n -> failwithf "invalid selected option: %d" n

                    currentTurn <- EnemyTurn playerAction

                    match playerAction with
                    | Attack ->
                        pushDialogue '6'
                    | Dodge ->
                        pushDialogue '7'
                    | Bark ->
                        pushDialogue '8'

            Continue

        | WonState ->
            Won

        | DiedState ->
            if keysJustPressed.Contains Keys.W || keysJustPressed.Contains Keys.Up then
                selectedOptionIdx <- ((selectedOptionIdx - 1) + 2) % 2
                selectSfx.Play() |> ignore

                Continue

            elif keysJustPressed.Contains Keys.S || keysJustPressed.Contains Keys.Down then
                selectedOptionIdx <- (selectedOptionIdx + 1) % 2
                selectSfx.Play() |> ignore

                Continue

            elif keysJustPressed.Contains Keys.Space || keysJustPressed.Contains Keys.E then
                actionSfx.Play () |> ignore
                
                match selectedOptionIdx with
                | 0 ->
                    actionQueue <- []
                    resetBattle (currentEnemyIndex + 1)
                    Continue

                | 1 -> Exit
                | n -> failwithf "invalid selected option: %d" n
            else
                Continue


    member _.Draw(spriteBatch: SpriteBatch) =
        let drawTexture tex (x, y) (w, h) =
            spriteBatch.Draw(tex, Rectangle(x, y, w, h), Color.White)

        let isShowingText = match state with TextState _ -> true | _ -> false

        if isShowingText then
            dialogueSystem.Draw(spriteBatch)

        let screenShakeAmount =
            if screenShake < 1 then
                0
            elif (screenShake / 3) % 2 = 0 then
                -5
            else
                5

        let drawHealthBar tex health y =
            let width =
                if health = 20 then
                    300
                else
                    20 + 13 * health

            spriteBatch.Draw(
                tex,
                Vector2(700.f + float32 screenShakeAmount, float32 y),
                Rectangle(0, 0, width, 50),
                Color.White
            )

        match state with
        | WonState -> ()
        | BattleState | TextState _ ->
            drawTexture battleUiTexture (0 + screenShakeAmount, 0) (1024, 768)

            if currentTurn = MyTurn && not isShowingText then
                drawTexture battleSelectorTexture.[battleSelectorIndex]
                    (701 + screenShakeAmount, 286 + (70 * selectedOptionIdx)) (50, 50)

            let enemyShakeAmount =
                if enemyShake < 1 then
                    0
                elif (enemyShake / 3) % 2 = 0 then
                        -3
                else
                    3
            drawTexture enemiesTexture.[currentEnemyIndex] (34 + enemyShakeAmount , 71) (600, 400)

            if yourHealth > 0 then
                drawHealthBar healthTexture.[healthBarIndex] yourHealth 25

            if currentEnemy.Health > 0 then
                drawHealthBar healthTexture.[(healthBarIndex + 1) % healthTextureCount] currentEnemy.Health 137

        | DiedState ->
            drawTexture youDiedTexture (0, 0) (1024, 768)

            drawTexture battleSelectorTexture.[battleSelectorIndex]
                (701 + screenShakeAmount, 286 + (70 * selectedOptionIdx)) (50, 50)
