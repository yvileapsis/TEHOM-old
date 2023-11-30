namespace Tehom

open System
open Prime
open Nu

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type MyGamePlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor
    override this.EditModes =
        Map.ofSeq [
            "Splash", Game.SetModel Splash
            "Title", Game.SetModel Title
            "Credits", Game.SetModel Credits
            "Gameplay", Game.SetModel (Gameplay Gameplay.makeDefault)
        ]