﻿namespace InfinityRpg
open System
open System.Numerics
open Prime
open Nu
open Nu.Declarative
open InfinityRpg

type [<ReferenceEquality; NoComparison>] MapModeler =
    { FieldMapUnits : Map<Vector2i, FieldMapUnit>
      CurrentFieldOffset : Vector2i }

    static member empty =
        { FieldMapUnits = Map.empty
          CurrentFieldOffset = v2iZero }

    member this.AddFieldMapUnit fieldMapUnit =
        let fieldMapUnits = Map.add fieldMapUnit.OffsetCount fieldMapUnit this.FieldMapUnits
        { this with FieldMapUnits = fieldMapUnits; CurrentFieldOffset = fieldMapUnit.OffsetCount }

    member this.Current =
        this.FieldMapUnits.[this.CurrentFieldOffset]

    member this.OffsetInDirection direction =
        this.CurrentFieldOffset + dtovc direction
    
    member this.ExistsInDirection direction =
        Map.containsKey (this.OffsetInDirection direction) this.FieldMapUnits
    
    member this.NextOffset =
        if this.Current.IsHorizontal
        then this.OffsetInDirection Rightward
        else this.OffsetInDirection Upward
    
    member this.NextOffsetInDirection direction =
        this.NextOffset = this.OffsetInDirection direction

    member this.PossibleInDirection direction =
        this.ExistsInDirection direction || this.NextOffsetInDirection direction
    
    member this.MoveCurrent direction =
        { this with CurrentFieldOffset = this.OffsetInDirection direction }
    
    member this.MakeFieldMapUnit =
        this.AddFieldMapUnit (FieldMapUnit.make (Some this.Current))
    
    member this.Transition direction =
        if this.ExistsInDirection direction
        then this.MoveCurrent direction
        else this.MakeFieldMapUnit
    
    static member make =
        MapModeler.empty.AddFieldMapUnit (FieldMapUnit.make None)

type [<NoComparison>] Move =
    | Step of Direction
    | Attack of CharacterIndex
    | Travel of NavigationNode list

    member this.TruncatePath =
        match this with
        | Travel (head :: _) -> Travel [head]
        | _ -> this

type [<ReferenceEquality; NoComparison>] FieldSpace =
    { CharacterOpt : Character Option
      PickupItemOpt : PickupType Option }

    member this.GetCharacter =
        match this.CharacterOpt with Some character -> character | None -> failwithumf ()
    
    member this.GetCharacterIndex =
        this.GetCharacter.CharacterIndex
    
    member this.ContainsCharacter =
        match this.CharacterOpt with Some _ -> true | None -> false

    member this.ContainsEnemy =
        match this.CharacterOpt with
        | None -> false // more efficient order
        | Some character -> character.IsEnemy

    static member containsEnemy (fieldSpace : FieldSpace) =
        fieldSpace.ContainsEnemy
    
    static member containsSpecifiedCharacter index fieldSpace =
        match fieldSpace.CharacterOpt with
        | None -> false // more efficient order
        | Some character -> character.CharacterIndex = index

    member this.ContainsPickup =
        match this.PickupItemOpt with Some _ -> true | None -> false
    
    static member containsPickup (fieldSpace : FieldSpace) =
        fieldSpace.ContainsPickup

    // internal
    
    static member updateCharacterOpt updater fieldSpace =
        { fieldSpace with CharacterOpt = updater fieldSpace.CharacterOpt }
    
    static member updatePickupItemOpt updater fieldSpace =
        { fieldSpace with PickupItemOpt = updater fieldSpace.PickupItemOpt }
    
    static member updateCharacterInternal updater characterOpt =
        match characterOpt with
        | Some character -> Some (updater character)
        | None -> failwithumf ()
    
    // interface
    
    static member addCharacter character fieldSpace =
        FieldSpace.updateCharacterOpt (constant (Some character)) fieldSpace
    
    static member updateCharacter updater fieldSpace =
        FieldSpace.updateCharacterOpt (FieldSpace.updateCharacterInternal updater) fieldSpace
    
    static member removeCharacter fieldSpace =
        FieldSpace.updateCharacterOpt (constant None) fieldSpace
    
    static member addPickup pickup fieldSpace =
        FieldSpace.updatePickupItemOpt (constant (Some pickup)) fieldSpace
    
    static member removePickup fieldSpace =
        FieldSpace.updatePickupItemOpt (constant None) fieldSpace
    
    static member empty =
        { CharacterOpt = None
          PickupItemOpt = None }

type [<ReferenceEquality; NoComparison>] Chessboard =
    { FieldSpaces : Map<Vector2i, FieldSpace> }

    // NOTE: the following subset data can be optimized on demand by converting them from filters to storage caches, without interfering with the interface.
    
    member this.CharacterSpaces =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsCharacter) this.FieldSpaces
    
    member this.EnemySpaces =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsEnemy) this.FieldSpaces

    member this.PickupSpaces =
        Map.filter (fun _ (v : FieldSpace) -> v.ContainsPickup) this.FieldSpaces

    member this.OccupiedSpaces =
        this.CharacterSpaces |> Map.toKeyList
    
    member this.UnoccupiedSpaces =
        Map.filter (fun _ (v : FieldSpace) -> not v.ContainsCharacter) this.FieldSpaces |> Map.toKeyList

    member this.OpenDirections coordinates =
        List.filter (fun d -> List.exists (fun x -> x = (coordinates + (dtovc d))) this.UnoccupiedSpaces) [Upward; Rightward; Downward; Leftward]

    member this.Characters =
        Map.map (fun _ (v : FieldSpace) -> v.GetCharacter) this.CharacterSpaces

    member this.EnemyIndices =
        Map.toListBy (fun _ (v : FieldSpace) -> v.GetCharacterIndex) this.EnemySpaces
    
    member this.EnemyCount =
        this.EnemySpaces.Count

    member this.PickupCount =
        this.PickupSpaces.Count
    
    
    
    static member updateFieldSpaces updater chessboard =
        { chessboard with FieldSpaces = updater chessboard.FieldSpaces }
    
    static member characterExists index chessboard =
        Map.exists (fun _ v -> FieldSpace.containsSpecifiedCharacter index v) chessboard.FieldSpaces
    
    static member pickupAtCoordinates coordinates chessboard =
        chessboard.FieldSpaces.[coordinates].ContainsPickup
    
    static member updateByCoordinatesInternal coordinates updater (fieldSpaces : Map<Vector2i, FieldSpace>) =
        Map.add coordinates (updater fieldSpaces.[coordinates]) fieldSpaces

    static member updateByPredicateInternal predicate updater (fieldSpaces : Map<Vector2i, FieldSpace>) =
        Map.map (fun _ v -> if predicate v then updater v else v) fieldSpaces
    
    static member updateByCoordinates coordinates updater chessboard =
        Chessboard.updateFieldSpaces (Chessboard.updateByCoordinatesInternal coordinates updater) chessboard
    
    static member updateByPredicate predicate updater chessboard =
        Chessboard.updateFieldSpaces (Chessboard.updateByPredicateInternal predicate updater) chessboard
    
    static member addHealth _ coordinates chessboard =
        Chessboard.updateByCoordinates coordinates (FieldSpace.addPickup Health) chessboard

    static member removePickup _ coordinates chessboard =
        Chessboard.updateByCoordinates coordinates FieldSpace.removePickup chessboard
    
    static member clearPickups _ _ chessboard =
        Chessboard.updateByPredicate FieldSpace.containsPickup FieldSpace.removePickup chessboard
    
    static member getCharacterCoordinates index chessboard =
        Map.findKey (fun _ v -> FieldSpace.containsSpecifiedCharacter index v) chessboard.FieldSpaces

    static member getCharacterAtCoordinates coordinates chessboard =
        chessboard.FieldSpaces.[coordinates].GetCharacter
    
    static member getCharacter index chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.getCharacterAtCoordinates coordinates chessboard
    
    static member addCharacter character coordinates (chessboard : Chessboard) =
        Chessboard.updateByCoordinates coordinates (FieldSpace.addCharacter character) chessboard

    static member updateCharacter index updater chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateByCoordinates coordinates (FieldSpace.updateCharacter updater) chessboard
    
    static member removeCharacter index _ chessboard =
        let coordinates = Chessboard.getCharacterCoordinates index chessboard
        Chessboard.updateByCoordinates coordinates FieldSpace.removeCharacter chessboard
    
    static member relocateCharacter index coordinates (chessboard : Chessboard) =
        let oldCoordinates = Chessboard.getCharacterCoordinates index chessboard
        let character = Chessboard.getCharacterAtCoordinates oldCoordinates chessboard
        let chessboard = Chessboard.updateByCoordinates oldCoordinates FieldSpace.removeCharacter chessboard
        Chessboard.addCharacter character coordinates chessboard
    
    static member clearEnemies _ _ (chessboard : Chessboard) =
        Chessboard.updateByPredicate FieldSpace.containsEnemy FieldSpace.removeCharacter chessboard
    
    static member setFieldSpaces _ fieldMap chessboard =
        let fieldSpaces = fieldMap.FieldTiles |> Map.filter (fun _ fieldTile -> fieldTile.TileType = Passable) |> Map.map (fun _ _ -> FieldSpace.empty)
        Chessboard.updateFieldSpaces (constant fieldSpaces) chessboard

    static member transitionMap _ fieldMap chessboard =
        let playerCoordinates = Chessboard.getCharacterCoordinates PlayerIndex chessboard
        let player = Chessboard.getCharacterAtCoordinates playerCoordinates chessboard
        let chessboard = Chessboard.setFieldSpaces () fieldMap chessboard
        Chessboard.addCharacter player playerCoordinates chessboard

    static member empty =
        { FieldSpaces = Map.empty }

type [<ReferenceEquality; NoComparison>] Gameplay =
    { ShallLoadGame : bool
      MapModeler : MapModeler
      Field : Field
      Chessboard : Chessboard
      CharacterMoves : Map<CharacterIndex, Move>
      CharacterTurns : Turn list }

    static member initial =
        { ShallLoadGame = false
          MapModeler = MapModeler.make
          Field = Field.initial
          Chessboard = Chessboard.empty
          CharacterMoves = Map.empty
          CharacterTurns = [] }

    static member updateMapModeler updater gameplay =
        { gameplay with MapModeler = updater gameplay.MapModeler }
    
    static member updateField updater gameplay =
        { gameplay with Field = updater gameplay.Field }
    
    // if updater takes index, index is arg1; if updater takes coordinates, coordinates is arg2
    static member updateChessboardBy updater arg1 arg2 gameplay =
        let chessboard = updater arg1 arg2 gameplay.Chessboard
        { gameplay with Chessboard = chessboard }
    
    static member updateCharacterMoves updater gameplay =
        { gameplay with CharacterMoves = updater gameplay.CharacterMoves }
    
    static member updateCharacterTurns updater gameplay =
        { gameplay with CharacterTurns = updater gameplay.CharacterTurns }
    
    static member getEnemyIndices gameplay =
        gameplay.Chessboard.EnemyIndices

    static member getOpponentIndices index gameplay =
        match index with
        | PlayerIndex -> Gameplay.getEnemyIndices gameplay
        | _ -> [PlayerIndex]
    
    static member anyTurnsInProgress gameplay = 
        List.notEmpty gameplay.CharacterTurns
    
    static member getCoordinates index gameplay =
        Chessboard.getCharacterCoordinates index gameplay.Chessboard

    static member getCharacter index gameplay =
        Chessboard.getCharacter index gameplay.Chessboard
    
    static member getIndexByCoordinates coordinates gameplay =
        let character = Chessboard.getCharacterAtCoordinates coordinates gameplay.Chessboard
        character.CharacterIndex
    
    static member getCharacterMove index gameplay =
        gameplay.CharacterMoves.[index]
    
    static member tryGetCharacterTurn index gameplay =
        List.tryFind (fun x -> x.Actor = index) gameplay.CharacterTurns
    
    static member getCharacterTurn index gameplay =
        List.find (fun x -> x.Actor = index) gameplay.CharacterTurns
    
    static member turnInProgress index gameplay =
        List.exists (fun x -> x.Actor = index) gameplay.CharacterTurns
    
    static member isPlayerAttacking gameplay =
        match Gameplay.tryGetCharacterTurn PlayerIndex gameplay with
        | Some turn -> turn.TurnType = AttackTurn
        | None -> false

    static member isPlayerTraveling gameplay =
        match Gameplay.tryGetCharacterTurn PlayerIndex gameplay with
        | Some turn ->
            match turn.TurnType with
            | WalkTurn multiRoundContext -> multiRoundContext
            | _ -> false
        | None -> false
    
    static member addMove index (move : Move) gameplay =
        let characterMoves = Map.add index move gameplay.CharacterMoves
        Gameplay.updateCharacterMoves (constant characterMoves) gameplay

    static member removeMove index gameplay =
        let characterMoves = Map.remove index gameplay.CharacterMoves
        Gameplay.updateCharacterMoves (constant characterMoves) gameplay
    
    static member truncatePlayerPath gameplay =
        let move = gameplay.CharacterMoves.[PlayerIndex].TruncatePath
        Gameplay.addMove PlayerIndex move gameplay
    
    static member updateCharacterTurn index updater gameplay =
        Gameplay.updateCharacterTurns (fun turns -> List.map (fun x -> if x.Actor = index then updater x else x) turns) gameplay
    
    static member setCharacterTurnStatus index status gameplay =
        Gameplay.updateCharacterTurn index (Turn.updateTurnStatus (constant status)) gameplay
    
    static member updateCharacter index updater gameplay =
        Gameplay.updateChessboardBy Chessboard.updateCharacter index updater gameplay

    static member relocateCharacter index coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.relocateCharacter index coordinates gameplay
    
    static member addHealth coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.addHealth () coordinates gameplay

    static member removeHealth coordinates gameplay =
        Gameplay.updateChessboardBy Chessboard.removePickup () coordinates gameplay
    
    static member clearPickups gameplay =
        Gameplay.updateChessboardBy Chessboard.clearPickups () () gameplay
    
    static member removeEnemy index gameplay =
        let coordinates = Gameplay.getCoordinates index gameplay
        let gameplay = Gameplay.addHealth coordinates gameplay
        Gameplay.updateChessboardBy Chessboard.removeCharacter index () gameplay

    static member clearEnemies gameplay =
        Gameplay.updateChessboardBy Chessboard.clearEnemies () () gameplay

    static member finishMove index gameplay =
        let gameplay = Gameplay.updateCharacterTurns (fun turns -> List.filter (fun x -> x.Actor <> index) turns) gameplay
        Gameplay.removeMove index gameplay
    
    static member tryPickupHealth index coordinates gameplay =
        match index with
        | PlayerIndex ->
            let gameplay = Gameplay.updateCharacter index (Character.updateHitPoints (constant 30)) gameplay
            Gameplay.removeHealth coordinates gameplay
        | _ -> gameplay
    
    static member applyStep index direction gameplay =
        let coordinates = (Gameplay.getCoordinates index gameplay) + dtovc direction
        let gameplay = Gameplay.updateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
        let gameplay =
            if Chessboard.pickupAtCoordinates coordinates gameplay.Chessboard then
                Gameplay.tryPickupHealth index coordinates gameplay
            else gameplay
        Gameplay.relocateCharacter index coordinates gameplay
    
    static member applyAttack index reactorIndex gameplay =
        let reactorDamage = 4 // NOTE: just hard-coding damage for now
        let coordinates = Gameplay.getCoordinates index gameplay
        let reactorCoordinates = Gameplay.getCoordinates reactorIndex gameplay
        let direction = Math.directionToTarget coordinates reactorCoordinates
        let gameplay = Gameplay.updateCharacter index (Character.updateFacingDirection (constant direction)) gameplay
        Gameplay.updateCharacter reactorIndex (Character.updateHitPoints (fun x -> x - reactorDamage)) gameplay
    
    static member stopTravelingPlayer reactorIndex gameplay =
        if reactorIndex = PlayerIndex then Gameplay.truncatePlayerPath gameplay else gameplay
    
    static member applyMove index gameplay =
        let move = Gameplay.getCharacterMove index gameplay
        match move with
        | Step direction -> Gameplay.applyStep index direction gameplay
        | Attack reactorIndex ->
            let gameplay = Gameplay.applyAttack index reactorIndex gameplay
            Gameplay.stopTravelingPlayer reactorIndex gameplay
        | Travel path ->
            match path with
            | head :: _ ->
                let currentCoordinates = Gameplay.getCoordinates index gameplay
                let direction = Math.directionToTarget currentCoordinates head.Coordinates
                Gameplay.applyStep index direction gameplay
            | [] -> failwithumf ()
    
    static member activateCharacter index gameplay =
        let move = Gameplay.getCharacterMove index gameplay
        let coordinates = Gameplay.getCoordinates index gameplay
        
        let turn =
            match move with
            | Step direction -> Turn.makeWalk index false coordinates direction
            | Attack reactorIndex ->
                let direction = Gameplay.getCoordinates reactorIndex gameplay |> Math.directionToTarget coordinates
                Turn.makeAttack index reactorIndex coordinates direction
            | Travel path ->
                let direction = Math.directionToTarget coordinates path.Head.Coordinates
                Turn.makeWalk index true coordinates direction

        Gameplay.updateCharacterTurns (fun x -> turn :: x) gameplay

    static member setFieldMap fieldMap gameplay =
        let gameplay = Gameplay.updateChessboardBy Chessboard.setFieldSpaces () fieldMap gameplay
        let field = { FieldMapNp = fieldMap }
        Gameplay.updateField (constant field) gameplay

    static member resetFieldMap fieldMap gameplay = // TODO: get rid of this redundency
        let gameplay = Gameplay.updateChessboardBy Chessboard.transitionMap () fieldMap gameplay
        let field = { FieldMapNp = fieldMap }
        Gameplay.updateField (constant field) gameplay
    
    static member transitionMap direction gameplay =
        let mapModeler = gameplay.MapModeler.Transition direction
        Gameplay.updateMapModeler (constant mapModeler) gameplay

    static member determineCharacterPosition index gameplay =
        match Gameplay.tryGetCharacterTurn index gameplay with
        | Some turn -> Turn.calculatePosition turn
        | None -> Gameplay.getCoordinates index gameplay |> vctovf

    static member makePlayer gameplay =
        Gameplay.updateChessboardBy Chessboard.addCharacter Character.makePlayer v2iZero gameplay

    static member makeEnemy index gameplay =
        let availableCoordinates = gameplay.Chessboard.UnoccupiedSpaces
        let coordinates = availableCoordinates.Item(Gen.random1 availableCoordinates.Length)
        Gameplay.updateChessboardBy Chessboard.addCharacter (Character.makeEnemy index) coordinates gameplay

    static member makeEnemies quantity gameplay =
        let rec recursion count gameplay =
            if count = quantity then gameplay
            else Gameplay.makeEnemy (EnemyIndex count) gameplay |> recursion (count + 1)
        recursion 0 gameplay
    
    static member forEachIndex updater indices gameplay =
        let rec recursion (indices : CharacterIndex list) gameplay =
            if indices.Length = 0 then gameplay
            else
                let index = indices.Head
                let gameplay = updater index gameplay
                recursion indices.Tail gameplay
        recursion indices gameplay