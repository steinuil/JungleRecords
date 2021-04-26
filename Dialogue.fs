module Deeper.Dialogue


open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Audio


type DialogueState = {
    Id: char
    Lines: int
    mutable CurrentLine: int
}


let textBoxCount = 2


type DialogueSystem(textBoxChangeHz) =
    let mutable dialogue = Unchecked.defaultof<DialogueState>


    let mutable textBoxChange = textBoxChangeHz
    let mutable textBoxIndex = 0

    let textBoxesBg = Array.create textBoxCount Unchecked.defaultof<Texture2D>
    let textBoxesFg = Array.create textBoxCount Unchecked.defaultof<Texture2D>
    let pressAnyKeyTexture = Array.create textBoxCount Unchecked.defaultof<Texture2D>

    let dialogues = MutableDict.empty ()

    let mutable sfx = Unchecked.defaultof<SoundEffect>


    member _.LoadContent(content: ContentManager) =
        for i in 1 .. textBoxCount do
            textBoxesBg.[i - 1] <- content.Load<Texture2D>(sprintf @"Content\Textures\text bg %d" i)
            textBoxesFg.[i - 1] <- content.Load<Texture2D>(sprintf @"Content\Textures\text fg %d" i)
            pressAnyKeyTexture.[i - 1] <- content.Load<Texture2D>(sprintf @"Content\Textures\press any key %d" i)

        let loadDialogue id lineCount =
            Array.init lineCount (fun i ->
                content.Load<Texture2D>(sprintf @"Content\Dialogue\%c %i" id (i + 1))
            )

        [
            // Dialogue
            'a', 1
            'b', 2
            'c', 2
            'd', 1

            // Records
            'u', 1
            'v', 1
            'w', 1
            'x', 1
            'y', 1
            'z', 1

            // Combat
            'm', 1
            'n', 1
            'o', 1
            'p', 1
            'q', 1
            'r', 1
            's', 1
            't', 1
            '0', 1
            '1', 1
            '2', 1
            '3', 1
            '4', 1
            '5', 1
            '6', 1
            '7', 1
            '8', 1
            '9', 1
            '+', 1
            '-', 1
        ]
        |> Seq.iter (fun (id, lineCount) ->
            let textures = loadDialogue id lineCount
            MutableDict.add id textures dialogues
        )

        let loadSfx name =
            content.Load<SoundEffect>(@"Content\Sounds\" + name)

        sfx <- loadSfx "dialogue"


    member _.SetDialogue(id: char) =
        dialogue <- {
            Id = id
            Lines = dialogues.[id].Length
            CurrentLine = 0
        }


    member _.Update(wasAnyKeyPressed) =
        if textBoxChange < 1 then
            textBoxChange <- textBoxChangeHz
            textBoxIndex <- (textBoxIndex + 1) % textBoxCount
        else
            textBoxChange <- textBoxChange - 1

        if wasAnyKeyPressed then
            sfx.Play() |> ignore
            if dialogue.CurrentLine >= dialogue.Lines - 1 then
                true
            else
                dialogue.CurrentLine <- dialogue.CurrentLine + 1
                false
        else
            false


    member _.Draw(spriteBatch: SpriteBatch) =
        let drawTexture (tex: Texture2D) =
            spriteBatch.Draw(tex, Rectangle(0, 0, 1024, 768), Color.White)

        drawTexture textBoxesBg.[textBoxIndex]
        drawTexture textBoxesFg.[textBoxIndex]
        drawTexture pressAnyKeyTexture.[textBoxIndex]

        drawTexture dialogues.[dialogue.Id].[dialogue.CurrentLine]
