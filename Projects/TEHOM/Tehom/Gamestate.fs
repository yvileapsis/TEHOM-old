module Gamestate
open Logic
open Tehon

let describePlayer = Actions.describeParent (ID "player")

let mutable gameWorldState: Result<World,Error> = Success gameWorld

let displayResult = function
    | Success s -> printf "%s" s
    | Failure f -> printf "%A" f

let Testing = 
    gameWorldState <-
    gameWorldState 
    >>= Actions.gameRegisterObject objectAbyss
    >>= Actions.gameRegisterObject worldSurface
    >>= Actions.gameRegisterObject key
    >>= foldBackBind Actions.gameRegisterObject allRooms
    >>= Actions.gameRegisterObject player

let Testing2 =
    gameWorldState
    >>= Actions.gameMoveObject (ID "player") (ID "center") (ID "north1")
    >>= Actions.describeParent (ID "player")
    |> ignore

// >>= Actions.gameMoveObject (ID "objectKey")  (ID "Lvl0Town") (ID "abyss")

    
type GameEvent =
    | UpdateState of Effects.Effect<World, World>
    | ResetState of World
    | EndGameLoop

let applyUpdate updateFunc worldState =
    match updateFunc worldState with
    | Success newState ->
        describePlayer newState |> displayResult
        newState
    | Failure message ->
        printfn "\n\n%s\n" message
        worldState

type GameEngine(initialState: World) =
    let gameLoop =
        MailboxProcessor.Start(fun inbox ->
            let rec innerLoop worldState =
                async {
                    let! eventMsg = inbox.Receive()
                    match eventMsg with
                    | UpdateState updateFunc -> return! innerLoop (applyUpdate updateFunc worldState)
                    | ResetState newState -> return! innerLoop newState
                    | EndGameLoop -> return ()
                }
                
            innerLoop initialState)

    member this.ApplyUpdate(updateFunc) =
        gameLoop.Post(UpdateState updateFunc)

    member this.ResetState(newState) =
        gameLoop.Post(ResetState newState)

    member this.Stop() =
        gameLoop.Post(EndGameLoop)

let gameEngine = GameEngine(gameWorld)
gameEngine.ApplyUpdate(move south)

let RunTehon() = 
    Success gameWorld
    >>= move south
    >>= describeCurrentRoom
    |> displayResult
    ()

RunTehon()