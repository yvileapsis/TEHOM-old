﻿namespace Tehom

open System
open System.IO
open Nu

module Program =

    // this the entry point for your Nu application
    let [<EntryPoint; STAThread>] main _ =

        // this specifies the window configuration used to display the game
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "TEHOM" }
        
        // this specifies the configuration of the game engine's use of SDL
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }

        // this specifies the world config using the above SDL config
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

        // point current working directory at program's base directory
        Directory.SetCurrentDirectory AppContext.BaseDirectory

        // initialize Nu
        Nu.init worldConfig.NuConfig

        // run the engine with the given config and plugin
        World.run worldConfig (MyGamePlugin ())