namespace Tehom

open System
open Prime
open Nu

open Entity
open Trait

[<AutoOpen>]
module Gameplay =

    // this represents that state of the simulation during gameplay.
    type GameplayState =
        | Playing
        | Quitting
        | Quit

    // this is our MMCC model type representing gameplay.
    // this model representation uses update time, that is, time based on number of engine updates.
    // if you wish to use clock time instead (https://github.com/bryanedds/Nu/wiki/GameTime-and-its-Polymorphic-Nature),
    // you could use `Time : single` instead.
    type Gameplay = {
        Time : int64
        State : GameplayState

        // I would like for this model to be possible to save/load and also move between screens
        Model : Model.Model

        Display : string
    } with
        static member makeDefault = {
            Time = 0
            State = Playing
            Model = YamlReader.FillModelWithYaml Model.Model.makeDefault
            Display = "Hello world!"
        }

    // this is our MMCC message type.
    type GameplayMessage =
        | Update
        | SetDisplayedString of string
        | SetDisplayedStringToEntityDescription of EntityID
        | StartQuitting
        | FinishQuitting
        interface Message

    // this extends the Screen API to expose the above Gameplay model.
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()

    // this is the screen dispatcher that defines the screen where gameplay takes place. Note that we just use the
    // empty Command type because there are no commands needed for this template.
    type GameplayDispatcher () =
        inherit ScreenDispatcher<Gameplay, GameplayMessage, Command> ({ Gameplay.makeDefault with State = Quitting })

        // here we define the screen's properties and event handling
        override this.Initialize (_, _) = [
            Screen.UpdateEvent => Update
            Screen.DeselectingEvent => FinishQuitting

            // no idea where to put debug test stuff, TODO: remove
            Screen.RenderEvent => SetDisplayedStringToEntityDescription (EntityID.ID "player")
        ]

        // here we handle the above messages
        override this.Message (gameplay, message, _, _) =
            match message with
            | Update ->
                just { gameplay with Time = inc gameplay.Time }
            | SetDisplayedString str ->
                just { gameplay with Display = str}
            | SetDisplayedStringToEntityDescription entityID ->
                let description =
                    match Map.tryFind entityID gameplay.Model.Description with
                    | Some description ->
                        match description.Description with
                        | String x -> x
                        | _ -> "did not find player description"
                    | _ -> "did not find player, weird!"
                just { gameplay with Display = description }
            | StartQuitting ->
                just { gameplay with State = Quitting }
            | FinishQuitting ->
                just { gameplay with State = Quit }

        // here we describe the content of the game including the level, the hud, and the player
        override this.Content (gameplay, _) = [// the gui group

            Content.group Simulants.GameplayGui.Name [] [// time
 
                Content.text Simulants.GameplayTime.Name [
                    Entity.Position == v3 0.0f 232.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Text := string gameplay.Time
                ]

                Content.text Simulants.GameplayTextBox.Name [
                    Entity.Position == v3 0.0f 132.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                    Entity.Text := gameplay.Display
                ]

                // quit
                Content.button Simulants.GameplayQuit.Name [
                    Entity.Position == v3 336.0f -216.0f 0.0f
                    Entity.Elevation == 10.0f
                    Entity.Text == "Quit"
                    Entity.ClickEvent => StartQuitting
                ]
            ]

            // the scene group while playing or quitting
            match gameplay.State with
            | Playing | Quitting ->
                Content.groupFromFile Simulants.GameplayScene.Name "Assets/Gameplay/Scene.nugroup" [] []
            | Quit -> ()]