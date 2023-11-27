﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open System.Numerics
open Prime

[<AutoOpen>]
module WorldModuleEntity =

    /// A reflective property getter.
    type internal PropertyGetter = Entity -> World -> Property

    /// A reflective property setter.
    type internal PropertySetter = Property -> Entity -> World -> struct (bool * World)

    /// Reflective property getters / setters.
    let internal EntityGetters = Dictionary<string, PropertyGetter> StringComparer.Ordinal
    let internal EntitySetters = Dictionary<string, PropertySetter> StringComparer.Ordinal

    /// Mutable clipboard that allows its state to persist beyond undo / redo.
    /// TODO: put this in AmbientState instead of leaving it free-floating?
    let mutable private Clipboard : obj option = None

    /// Publishing IDs.
    let internal EntityChangeCountsId = Gen.id
    let internal EntityBindingCountsId = Gen.id

    // OPTIMIZATION: avoids closure allocation in tight-loop.
    type private KeyEquality () =
        inherit OptimizedClosures.FSharpFunc<
            KeyValuePair<Entity, SUMap<Entity, EntityState>>,
            KeyValuePair<Entity, SUMap<Entity, EntityState>>,
            bool> ()
        override this.Invoke _ = failwithumf ()
        override this.Invoke
            (entityStateKey : KeyValuePair<Entity, SUMap<Entity, EntityState>>,
             entityStateKey2 : KeyValuePair<Entity, SUMap<Entity, EntityState>>) =
            refEq entityStateKey.Key entityStateKey2.Key &&
            refEq entityStateKey.Value entityStateKey2.Value
    let private keyEquality = KeyEquality ()

    // OPTIMIZATION: avoids closure allocation in tight-loop.
    let mutable private getFreshKeyAndValueEntity = Unchecked.defaultof<Entity>
    let mutable private getFreshKeyAndValueWorld = Unchecked.defaultof<World>
    let private getFreshKeyAndValue () =
        let mutable entityStateOpt = Unchecked.defaultof<_>
        let _ = SUMap.tryGetValue (getFreshKeyAndValueEntity, getFreshKeyAndValueWorld.EntityStates, &entityStateOpt)
        KeyValuePair (KeyValuePair (getFreshKeyAndValueEntity, getFreshKeyAndValueWorld.EntityStates), entityStateOpt)
    let private getFreshKeyAndValueCached =
        getFreshKeyAndValue

    // OPTIMIZATION: cache one entity change address to reduce allocation where possible.
    let mutable changeEventNamesFree = true
    let changeEventNamesCached = [|Constants.Lens.ChangeName; ""; Constants.Lens.EventName; ""; ""; ""; ""|]

    type World with

        // OPTIMIZATION: a ton of optimization has gone down in here...!
        static member private entityStateRefresher (entity : Entity) world =
            getFreshKeyAndValueEntity <- entity
            getFreshKeyAndValueWorld <- world
            let entityStateOpt =
                KeyedCache.getValueFast
                    keyEquality
                    getFreshKeyAndValueCached
                    (KeyValuePair (entity, world.EntityStates))
                    (World.getEntityCachedOpt world)
            getFreshKeyAndValueEntity <- Unchecked.defaultof<Entity>
            getFreshKeyAndValueWorld <- Unchecked.defaultof<World>
            match entityStateOpt :> obj with
            | null ->
                Unchecked.defaultof<EntityState>
            | _ ->
                if entityStateOpt.Imperative then entity.EntityStateOpt <- entityStateOpt
                entityStateOpt

        static member private entityStateFinder (entity : Entity) world =
            let entityStateOpt = entity.EntityStateOpt
            if isNull (entityStateOpt :> obj) || entityStateOpt.Invalidated
            then World.entityStateRefresher entity world
            else entityStateOpt

        static member private entityStateAdder entityState (entity : Entity) world =
            let parent =
                if entity.EntityAddress.Names.Length <= 4
                then entity.Group :> Simulant
                else Entity (Array.allButLast entity.EntityAddress.Names) :> Simulant
            let simulants =
                match world.Simulants.TryGetValue parent with
                | (true, entitiesOpt) ->
                    match entitiesOpt with
                    | Some entities ->
                        let entities = USet.add (entity :> Simulant) entities
                        UMap.add parent (Some entities) world.Simulants
                    | None ->
                        let entities = USet.singleton HashIdentity.Structural (World.getCollectionConfig world) (entity :> Simulant)
                        UMap.add parent (Some entities) world.Simulants
                | (false, _) -> failwith ("Cannot add entity '" + scstring entity + "' to non-existent parent '" + scstring parent + "'.")
            let simulants =
                if not (UMap.containsKey (entity :> Simulant) simulants)
                then UMap.add (entity :> Simulant) None simulants
                else simulants
            let entityStates = SUMap.add entity entityState world.EntityStates
            World.choose { world with Simulants = simulants; EntityStates = entityStates }

        static member private entityStateRemover (entity : Entity) world =
            let parent =
                if entity.EntityAddress.Names.Length <= 4
                then entity.Group :> Simulant
                else Entity (Array.allButLast entity.EntityAddress.Names) :> Simulant
            let simulants =
                match world.Simulants.TryGetValue parent with
                | (true, entitiesOpt) ->
                    match entitiesOpt with
                    | Some entities ->
                        let entities = USet.remove (entity :> Simulant) entities
                        if USet.isEmpty entities
                        then UMap.add parent None world.Simulants
                        else UMap.add parent (Some entities) world.Simulants
                    | None -> world.Simulants
                | (false, _) -> world.Simulants
            let simulants = UMap.remove (entity :> Simulant) simulants
            let entityStates = SUMap.remove entity world.EntityStates
            World.choose { world with Simulants = simulants; EntityStates = entityStates }

        static member private entityStateSetter entityState (entity : Entity) world =
#if DEBUG
            if not (SUMap.containsKey entity world.EntityStates) then
                failwith ("Cannot set the state of a non-existent entity '" + scstring entity + "'")
#endif
            let entityStates = SUMap.add entity entityState world.EntityStates
            World.choose { world with EntityStates = entityStates }

        static member private addEntityState entityState (entity : Entity) world =
            World.synchronizeEntityState entityState entity world
            World.entityStateAdder entityState entity world

        static member private removeEntityState (entity : Entity) world =
            World.entityStateRemover entity world

        static member private publishEntityChange propertyName (previousValue : obj) (propertyValue : obj) publishChangeEvents (entity : Entity) world =
            if publishChangeEvents then
                let changeData = { Name = propertyName; Previous = previousValue; Value = propertyValue }
                let entityNames = Address.getNames entity.EntityAddress
                let mutable changeEventNamesUtilized = false
                let changeEventAddress =
                    // OPTIMIZATION: this optimization should be hit >= 90% of the time. The 10% of cases where
                    // it isn't should be acceptable.
                    if  Array.length entityNames = 4 &&
                        changeEventNamesFree then
                        changeEventNamesFree <- false
                        changeEventNamesUtilized <- true
                        changeEventNamesCached.[1] <- propertyName
                        changeEventNamesCached.[3] <- entityNames.[0]
                        changeEventNamesCached.[4] <- entityNames.[1]
                        changeEventNamesCached.[5] <- entityNames.[2]
                        changeEventNamesCached.[6] <- entityNames.[3]
                        rtoa<ChangeData> changeEventNamesCached
                    else rtoa<ChangeData> (Array.append [|Constants.Lens.ChangeName; propertyName; Constants.Lens.EventName|] entityNames)
                let eventTrace = EventTrace.debug "World" "publishEntityChange" "" EventTrace.empty
                let world = World.publishPlus changeData changeEventAddress eventTrace entity false false world
                if changeEventNamesUtilized then changeEventNamesFree <- true
                world
            else world

        static member inline private getEntityStateOpt entity world =
            World.entityStateFinder entity world

#if DEBUG
        static member internal getEntityState entity world =
            let entityStateOpt = World.entityStateFinder entity world
            match entityStateOpt :> obj with
            | null -> failwith ("Could not find entity '" + scstring entity + "'.")
            | _ -> entityStateOpt
#else
        static member inline internal getEntityState entity world =
            World.entityStateFinder entity world
#endif

        static member internal getEntityXtensionProperties entity world =
            let entityState = World.getEntityState entity world
            entityState.Xtension |> Xtension.toSeq |> Seq.toList

        static member private synchronizeEntityState (entityState : EntityState) (entity : Entity) world =

            // grab address
            let entityAddress = entity.EntityAddress
            
            // apply publish changes state
            match World.tryGetKeyedValueFast<UMap<Entity Address, int>> (EntityChangeCountsId, world) with
            | (true, entityChangeCounts) -> if UMap.containsKey entityAddress entityChangeCounts then entityState.PublishChangeEvents <- true
            | (false, _) -> ()
            
            // apply mounted state
            entityState.Mounted <- UMap.containsKey entity world.EntityMounts

        static member inline internal setEntityState entityState entity world =
            World.entityStateSetter entityState entity world

        static member private publishEntityChanges entity world =
            let entityState = World.getEntityState entity world
            let properties = World.getSimulantStateProperties entityState
            let publishChangeEvents = entityState.PublishChangeEvents
            if publishChangeEvents then
                List.fold (fun world (propertyName, _, propertyValue) ->
                    let entityState = World.getEntityState entity world
                    let publishChangeEvents = entityState.PublishChangeEvents
                    World.publishEntityChange propertyName propertyValue propertyValue publishChangeEvents entity world)
                    world properties
            else world

        static member internal publishTransformEvents (transformOld : Transform byref, transformNew : Transform byref, publishChangeEvents, entity : Entity, world) =
            if publishChangeEvents then
                let positionChanged = v3Neq transformNew.Position transformOld.Position
                let scaleChanged = v3Neq transformNew.Scale transformOld.Scale
                let offsetChanged = v3Neq transformNew.Offset transformOld.Offset
                let rotationChanged = quatNeq transformNew.Rotation transformOld.Rotation
                let sizeChanged = v3Neq transformNew.Size transformOld.Size
                let elevationChanged = transformNew.Elevation <> transformOld.Elevation
                let overflowChanged = transformNew.Overflow <> transformOld.Overflow
                let centeredChanged = transformNew.Centered <> transformOld.Centered
                let perimeterChanged = positionChanged || scaleChanged || offsetChanged || sizeChanged || centeredChanged
                // OPTIMIZATION: eliding data for computed change events for speed.
                let world = World.publishEntityChange (nameof Transform) () () publishChangeEvents entity world
                let world =
                    if perimeterChanged then
                        let world = World.publishEntityChange (nameof transformNew.Bounds) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.PerimeterOriented) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.Perimeter) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.PerimeterUnscaled) () () publishChangeEvents entity world
                        let world = if positionChanged then World.publishEntityChange (nameof transformNew.Position) transformOld.Position transformNew.Position publishChangeEvents entity world else world
                        let world = World.publishEntityChange (nameof transformNew.Center) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.Bottom) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.BottomLeft) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.Min) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.Max) () () publishChangeEvents entity world
                        let world = if scaleChanged then World.publishEntityChange (nameof transformNew.Scale) transformOld.Scale transformNew.Scale publishChangeEvents entity world else world
                        let world = if offsetChanged then World.publishEntityChange (nameof transformNew.Offset) transformOld.Offset transformNew.Offset publishChangeEvents entity world else world
                        let world = if sizeChanged then World.publishEntityChange (nameof transformNew.Size) transformOld.Size transformNew.Size publishChangeEvents entity world else world
                        let world = if centeredChanged then World.publishEntityChange (nameof transformNew.Centered) transformOld.Centered transformNew.Centered publishChangeEvents entity world else world
                        world
                    else world
                let world =
                    if rotationChanged then
                        let world = World.publishEntityChange (nameof transformNew.Rotation) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.Angles) () () publishChangeEvents entity world
                        let world = World.publishEntityChange (nameof transformNew.Degrees) () () publishChangeEvents entity world
                        world
                    else world
                let world =
                    if elevationChanged
                    then World.publishEntityChange (nameof transformNew.Elevation) transformOld.Elevation transformNew.Elevation publishChangeEvents entity world
                    else world
                let world =
                    if overflowChanged
                    then World.publishEntityChange (nameof transformNew.Overflow) transformOld.Overflow transformNew.Overflow publishChangeEvents entity world
                    else world
                world
            else world

        static member internal getEntityExists entity world =
            notNull (World.getEntityStateOpt entity world :> obj)

        static member internal getEntityImperative entity world =
            (World.getEntityState entity world).Imperative

        static member internal setEntityImperative value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Imperative
            if value <> previous then
                let struct (entityState, world) =
                    if value then
                        let properties = UMap.makeFromSeq StringComparer.Ordinal Imperative (Xtension.toSeq entityState.Xtension)
                        let xtension = Xtension.make true properties
                        entityState.Xtension <- xtension
                        entityState.Imperative <- true
                        struct (entityState, world)
                    else
                        let properties = UMap.makeFromSeq StringComparer.Ordinal Functional (Xtension.toSeq entityState.Xtension)
                        let xtension = Xtension.make false properties
                        let entityState = EntityState.diverge entityState
                        entityState.Xtension <- xtension
                        entityState.Imperative <- false
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.Imperative) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal getEntityModelProperty entity world =
            let entityState = World.getEntityState entity world
            entityState.Model

        static member internal setEntityModelProperty initializing (value : DesignerProperty) entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Model
            if value.DesignerValue =/= previous.DesignerValue || initializing then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Model <- { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }
                        struct (entityState, world)
                    else
                        let entityState = { entityState with Model = { DesignerType = value.DesignerType; DesignerValue = value.DesignerValue }}
                        struct (entityState, World.setEntityState entityState entity world)
                let world = entityState.Dispatcher.TrySynchronize (initializing, entity, world)
                let world = World.publishEntityChange Constants.Engine.ModelPropertyName previous.DesignerValue value.DesignerValue entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal getEntityModel<'a> entity world =
            let entityState = World.getEntityState entity world
            match entityState.Model.DesignerValue with
            | :? 'a as model -> model
            | null -> null :> obj :?> 'a
            | modelObj ->
                try modelObj |> valueToSymbol |> symbolToValue
                with _ ->
                    Log.debugOnce "Could not convert existing model to new type. Falling back on initial model value."
                    match entityState.Dispatcher.TryGetInitialModel<'a> world with
                    | None -> failwithnie ()
                    | Some value -> value

        static member internal setEntityModel<'a> initializing (value : 'a) entity world =
            let entityState = World.getEntityState entity world
            let valueObj = value :> obj
            let previous = entityState.Model
            if valueObj =/= previous.DesignerValue || initializing then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Model <- { DesignerType = typeof<'a>; DesignerValue = valueObj }
                        struct (entityState, world)
                    else
                        let entityState = { entityState with Model = { DesignerType = typeof<'a>; DesignerValue = valueObj }}
                        struct (entityState, World.setEntityState entityState entity world)
                let world = entityState.Dispatcher.TrySynchronize (initializing, entity, world)
                let world = World.publishEntityChange Constants.Engine.ModelPropertyName previous.DesignerValue value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal getEntityContent entity world =
            let entityState = World.getEntityState entity world
            entityState.Content

        static member internal setEntityContent value entity world =
            let entityState = World.getEntityState entity world
            if entityState.Imperative then
                entityState.Content <- value
                world
            else
                let entityState = { entityState with Content = value }
                World.setEntityState entityState entity world

        static member internal getEntityDispatcher entity world = (World.getEntityState entity world).Dispatcher
        static member internal getEntityFacets entity world = (World.getEntityState entity world).Facets
        static member internal getEntityPosition entity world = (World.getEntityState entity world).Position
        static member internal getEntityCenter entity world = (World.getEntityState entity world).Transform.Center
        static member internal getEntityBottom entity world = (World.getEntityState entity world).Transform.Bottom
        static member internal getEntityBottomLeft entity world = (World.getEntityState entity world).Transform.BottomLeft
        static member internal getEntityMin entity world = (World.getEntityState entity world).Transform.Min
        static member internal getEntityMax entity world = (World.getEntityState entity world).Transform.Max
        static member internal getEntityPositionLocal entity world = (World.getEntityState entity world).PositionLocal
        static member internal getEntityCenterLocal entity world = (World.getEntityState entity world).CenterLocal
        static member internal getEntityBottomLocal entity world = (World.getEntityState entity world).BottomLocal
        static member internal getEntityBottomLeftLocal entity world = (World.getEntityState entity world).BottomLeftLocal
        static member internal getEntityMinLocal entity world = (World.getEntityState entity world).MinLocal
        static member internal getEntityMaxLocal entity world = (World.getEntityState entity world).MaxLocal
        static member internal getEntityRotation entity world = (World.getEntityState entity world).Rotation
        static member internal getEntityRotationLocal entity world = (World.getEntityState entity world).RotationLocal
        static member internal getEntityScale entity world = (World.getEntityState entity world).Scale
        static member internal getEntityScaleLocal entity world = (World.getEntityState entity world).ScaleLocal
        static member internal getEntityOffset entity world = (World.getEntityState entity world).Offset
        static member internal getEntityAngles entity world = (World.getEntityState entity world).Angles
        static member internal getEntityAnglesLocal entity world = (World.getEntityState entity world).AnglesLocal
        static member internal getEntityDegrees entity world = (World.getEntityState entity world).Degrees
        static member internal getEntityDegreesLocal entity world = Math.RadiansToDegrees3d (World.getEntityState entity world).AnglesLocal
        static member internal getEntitySize entity world = (World.getEntityState entity world).Size
        static member internal getEntityElevation entity world = (World.getEntityState entity world).Elevation
        static member internal getEntityElevationLocal entity world = (World.getEntityState entity world).ElevationLocal
        static member internal getEntityOverflow entity world = (World.getEntityState entity world).Transform.Overflow
        static member internal getEntityPresence entity world = (World.getEntityState entity world).Presence
        static member internal getEntityAbsolute entity world = (World.getEntityState entity world).Absolute
        static member internal getEntityPublishChangeEvents entity world = (World.getEntityState entity world).PublishChangeEvents
        static member internal getEntityEnabled entity world = (World.getEntityState entity world).Enabled
        static member internal getEntityEnabledLocal entity world = (World.getEntityState entity world).EnabledLocal
        static member internal getEntityVisible entity world = (World.getEntityState entity world).Visible
        static member internal getEntityVisibleLocal entity world = (World.getEntityState entity world).VisibleLocal
        static member internal getEntityPickable entity world = (World.getEntityState entity world).Pickable
        static member internal getEntityAlwaysUpdate entity world = (World.getEntityState entity world).AlwaysUpdate
        static member internal getEntityAlwaysRender entity world = (World.getEntityState entity world).AlwaysRender
        static member internal getEntityPublishPreUpdates entity world = (World.getEntityState entity world).PublishPreUpdates
        static member internal getEntityPublishUpdates entity world = (World.getEntityState entity world).PublishUpdates
        static member internal getEntityPublishPostUpdates entity world = (World.getEntityState entity world).PublishPostUpdates
        static member internal getEntityPublishRenders entity world = (World.getEntityState entity world).PublishRenders
        static member internal getEntityProtected entity world = (World.getEntityState entity world).Protected
        static member internal getEntityPersistent entity world = (World.getEntityState entity world).Persistent
        static member internal getEntityMounted entity world = (World.getEntityState entity world).Mounted
        static member internal getEntityIs2d entity world = (World.getEntityState entity world).Is2d
        static member internal getEntityIs3d entity world = (World.getEntityState entity world).Is3d
        static member internal getEntityCentered entity world = (World.getEntityState entity world).Centered
        static member internal getEntityStatic entity world = (World.getEntityState entity world).Static
        static member internal getEntityLightProbe entity world = (World.getEntityState entity world).LightProbe
        static member internal getEntityLight entity world = (World.getEntityState entity world).Light
        static member internal getEntityPhysical entity world = (World.getEntityState entity world).Physical
        static member internal getEntityOptimized entity world = (World.getEntityState entity world).Optimized
        static member internal getEntityShouldMutate entity world = (World.getEntityState entity world).Imperative
        static member internal getEntityDestroying (entity : Entity) world = List.exists ((=) (entity :> Simulant)) (World.getDestructionListRev world)
        static member internal getEntityMountOpt entity world = (World.getEntityState entity world).MountOpt
        static member internal getEntityFacetNames entity world = (World.getEntityState entity world).FacetNames
        static member internal getEntityOverlayNameOpt entity world = (World.getEntityState entity world).OverlayNameOpt
        static member internal getEntityOrder entity world = (World.getEntityState entity world).Order
        static member internal getEntityId entity world = (World.getEntityState entity world).Id
        static member internal getEntitySurnames entity world = (World.getEntityState entity world).Surnames
        static member internal getEntityName entity world = (World.getEntityState entity world).Surnames |> Array.last

        static member internal setEntityPublishChangeEvents value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PublishChangeEvents
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.PublishChangeEvents <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.PublishChangeEvents <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.PublishChangeEvents) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)
        
        static member internal setEntityPublishPreUpdates value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PublishPreUpdates
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.PublishPreUpdates <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.PublishPreUpdates <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.PublishPreUpdates) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)
        
        static member internal setEntityPublishUpdates value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PublishUpdates
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.PublishUpdates <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.PublishUpdates <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.PublishUpdates) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)
        
        static member internal setEntityPublishPostUpdates value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PublishPostUpdates
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.PublishPostUpdates <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.PublishPostUpdates <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.PublishPostUpdates) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)
        
        static member internal setEntityPublishRenders value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.PublishRenders
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.PublishRenders <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.PublishRenders <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.PublishRenders) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityProtected value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Protected
            if value <> previous then
                if entityState.Imperative then
                    entityState.Protected <- value
                    struct (true, world)
                else
                    let entityState = EntityState.diverge entityState
                    entityState.Protected <- value
                    struct (true, World.setEntityState entityState entity world)
            else struct (false, world)
        
        static member internal setEntityPersistent value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Persistent
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Persistent <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.Persistent <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.Persistent) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)
        
        static member internal setEntityMounted value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Mounted
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Mounted <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.Mounted <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.Mounted) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)
        
        static member internal setEntityOrder value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Order
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Order <- value
                        struct (entityState, world)
                    else
                        let entityState = { entityState with Order = value }
                        (entityState, World.setEntityState entityState entity world)
                let world = World.publishEntityChange (nameof entityState.Order) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member inline internal getEntityRotationMatrix entity world =
            (World.getEntityState entity world).Transform.RotationMatrix

        static member inline internal getEntityAffineMatrix entity world =
            (World.getEntityState entity world).Transform.AffineMatrix

        static member inline internal getEntityAffineMatrixOffset entity world =
            (World.getEntityState entity world).Transform.AffineMatrixOffset
        
        static member internal getEntityAffineMatrixLocal entity world =
            let entityState = World.getEntityState entity world
            Matrix4x4.CreateFromTrs (entityState.PositionLocal, entityState.RotationLocal, entityState.ScaleLocal)

        static member
#if !DEBUG
            inline
#endif
            internal getEntityTransform entity world =
            let entityState = World.getEntityState entity world
            let transform = &entityState.Transform
            transform.CleanRotationMatrix () // OPTIMIZATION: ensure rotation matrix is clean so that redundant cleans don't happen when transform is handed out.
            transform

        /// Get all of the entities directly parented by an entity.
        static member getEntityChildren (entity : Entity) world =
            let simulants = World.getSimulants world
            match simulants.TryGetValue (entity :> Simulant) with
            | (true, entitiesOpt) ->
                match entitiesOpt with
                | Some entities -> entities |> Seq.map cast<Entity> |> seq
                | None -> Seq.empty
            | (false, _) -> Seq.empty

        /// Traverse all of the entities directly parented by an entity.
        static member traverseEntityChildren effect (entity : Entity) (world : World) =
            let mounters = World.getEntityChildren entity world
            Seq.fold (fun world mounter -> effect entity mounter world) world mounters

        /// Get all of the entities directly mounted on an entity.
        static member getEntityMounters entity world =
            match world.EntityMounts.TryGetValue entity with
            | (true, mounters) -> Seq.filter (flip World.getEntityExists world) mounters |> SList.ofSeq |> seq
            | (false, _) -> Seq.empty

        /// Traverse all of the entities directly mounted on an entity.
        static member traverseEntityMounters effect (entity : Entity) (world : World) =
            let mounters = World.getEntityMounters entity world
            Seq.fold (fun world mounter -> effect entity mounter world) world mounters

        static member internal addEntityToMounts mountOpt entity world =
            match Option.bind (tryResolve entity) mountOpt with
            | Some mountNew ->
                let world =
                    match world.EntityMounts.TryGetValue mountNew with
                    | (true, mounters) ->
                        let mounters = USet.add entity mounters
                        let world = { world with EntityMounts = UMap.add mountNew mounters world.EntityMounts }
                        world
                    | (false, _) ->
                        let mounters = USet.singleton HashIdentity.Structural (World.getCollectionConfig world) entity
                        let world = World.choose { world with EntityMounts = UMap.add mountNew mounters world.EntityMounts }
                        let world = if World.getEntityExists mountNew world then World.setEntityMounted true mountNew world |> snd' else world
                        world
                let mountData = { Mount = mountNew; Mounter = entity }
                let eventTrace = EventTrace.debug "World" "addEntityToMounts" "" EventTrace.empty
                World.publishPlus mountData (Events.MountEvent --> mountNew) eventTrace entity false false world
            | None -> world

        static member internal removeEntityFromMounts mountOpt entity world =
            match Option.bind (tryResolve entity) mountOpt with
            | Some mountOld ->
                let world =
                    match world.EntityMounts.TryGetValue mountOld with
                    | (true, mounters) ->
                        let mounters = USet.remove entity mounters
                        if USet.isEmpty mounters then
                            let world = World.choose { world with EntityMounts = UMap.remove mountOld world.EntityMounts }
                            let world = if World.getEntityExists mountOld world then World.setEntityMounted false mountOld world |> snd' else world
                            world
                        else World.choose { world with EntityMounts = UMap.add mountOld mounters world.EntityMounts }
                    | (false, _) -> world
                let mountData = { Mount = mountOld; Mounter = entity }
                let eventTrace = EventTrace.debug "World" "removeEntityFromMounts" "" EventTrace.empty
                World.publishPlus mountData (Events.UnmountEvent --> mountOld) eventTrace entity false false world
            | None -> world

        static member internal propagateEntityAffineMatrix3 mount mounter world =
            let mounterState = World.getEntityState mounter world
            if world.Halted || not mounterState.Physical then
                let affineMatrixWorld = World.getEntityAffineMatrix mount world
                let affineMatrixLocal = World.getEntityAffineMatrixLocal mounter world
                let affineMatrix = affineMatrixLocal * affineMatrixWorld
                let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                if Matrix4x4.Decompose (affineMatrix, &scale, &rotation, &position) then
                    let mutable transform = mounterState.Transform
                    transform.Position <- position
                    transform.Rotation <- rotation
                    transform.Scale <- scale
                    World.setEntityTransformByRef (&transform, mounterState, mounter, world) |> snd'
                else world
            else world

        static member internal propagateEntityProperties3 mountOpt entity world =
            match Option.bind (tryResolve entity) mountOpt with
            | Some mountNew when World.getEntityExists mountNew world ->
                let world = World.propagateEntityAffineMatrix3 mountNew entity world
                let world = World.propagateEntityElevation3 mountNew entity world
                let world = World.propagateEntityEnabled3 mountNew entity world
                let world = World.propagateEntityVisible3 mountNew entity world
                world
            | _ -> world

        static member internal propagateEntityAffineMatrix entity world =
            World.traverseEntityMounters World.propagateEntityAffineMatrix3 entity world

        static member internal setEntityMountOpt value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.MountOpt
            if value <> previous then

                // update property
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.MountOpt <- value
                        struct (entityState, world)
                    else
                        let entityState = { entityState with MountOpt = value }
                        struct (entityState, World.setEntityState entityState entity world)

                // update mount hierarchy
                let world = World.removeEntityFromMounts previous entity world
                let world = World.addEntityToMounts value entity world

                // propagate properties from mount
                let world = World.propagateEntityProperties3 value entity world

                // publish change event unconditionally
                let world = World.publishEntityChange (nameof entityState.MountOpt) previous value true entity world

                // publish life cycle event unconditionally
                let eventTrace = EventTrace.debug "World" "setEntityMount" "" EventTrace.empty
                let world = World.publishPlus (MountOptChangeData (previous, value, entity)) (Events.LifeCycleEvent (nameof Entity) --> Nu.Game.Handle) eventTrace entity false false world
                struct (true, world)

            else struct (false, world)

        static member internal setEntityAbsolute value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Absolute
            if value <> previous then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Absolute <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.Absolute <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChange (nameof entityState.Absolute) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityStatic value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Static
            if value <> previous then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Static <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.Static <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChange (nameof entityState.Static) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityAlwaysUpdate value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.AlwaysUpdate
            if value <> previous then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.AlwaysUpdate <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.AlwaysUpdate <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChange (nameof entityState.AlwaysUpdate) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityAlwaysRender value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.AlwaysRender
            if value <> previous then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.AlwaysRender <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.AlwaysRender <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChange (nameof entityState.AlwaysRender) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityLightProbe value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.LightProbe
            if value <> previous then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.LightProbe <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.LightProbe <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChange (nameof entityState.LightProbe) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityLight value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Light
            if value <> previous then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Light <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.Light <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChange (nameof entityState.Light) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityPresence (value : Presence) entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Presence
            if presenceNeq value previous && (value.OmnipresentType || not entityState.Absolute) then // a transform that is Absolute must remain Omnipresent then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Presence <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.Presence <- value
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChange (nameof entityState.Presence) previous value entityState.PublishChangeEvents entity world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityTransformByRefWithoutEvent (value : Transform inref, entityState : EntityState, entity : Entity, world) =
            if not (Transform.equalsByRef (&value, &entityState.Transform)) then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let world =
                    if entityState.Imperative then
                        entityState.Transform <- value
                        world
                    else
                        let entityState = { entityState with Transform = value }
                        World.setEntityState entityState entity world
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world else world
            else world

        static member internal setEntityTransformByRef (value : Transform byref, entityState : EntityState, entity : Entity, world) =
            let mutable previous = entityState.Transform
            if not (Transform.equalsByRef (&value, &previous)) then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Transform <- value
                        struct (entityState, world)
                    else
                        let entityState = { entityState with Transform = value }
                        struct (entityState, World.setEntityState entityState entity world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = if World.getEntityMounted entity world then World.propagateEntityAffineMatrix entity world else world
                let world = World.publishTransformEvents (&previous, &value, entityState.PublishChangeEvents, entity, world)
                struct (true, world)
            else struct (false, world)

        static member internal setEntityPosition value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Position then
                if entityState.Optimized then
                    entityState.Position <- value
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Position <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityCenter value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.Center
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityBottom value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.Bottom
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityBottomLeft value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.BottomLeft
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityMin value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.Min
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityMax value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.Position - entityState.Max
            World.setEntityPosition (value + offset) entity world

        static member internal setEntityPositionLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.PositionLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.PositionLocal <- value
                    let position =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let affineMatrix = World.getEntityAffineMatrix mount world
                            Vector3.Transform (value, affineMatrix)
                        | _ -> value
                    entityState.Position <- position
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)

                else // do updates and propagation out-of-place

                    // update PositionLocal property
                    let struct (entityState, world) =
                        let previous = entityState.PositionLocal
                        if v3Neq value previous then
                            let centerPrevious = entityState.CenterLocal
                            let bottomPrevious = entityState.BottomLocal
                            let bottomLeftPrevious = entityState.BottomLeftLocal
                            let minPrevious = entityState.MinLocal
                            let maxPrevious = entityState.MaxLocal
                            let struct (entityState, world) =
                                if entityState.Imperative then
                                    entityState.PositionLocal <- value
                                    struct (entityState, world)
                                else
                                    let entityState = { entityState with PositionLocal = value }
                                    struct (entityState, World.setEntityState entityState entity world)
                            if entityState.PublishChangeEvents then
                                let world = World.publishEntityChange (nameof entityState.PositionLocal) previous value true entity world
                                let world = World.publishEntityChange (nameof entityState.CenterLocal) centerPrevious entityState.CenterLocal true entity world
                                let world = World.publishEntityChange (nameof entityState.BottomLocal) bottomPrevious entityState.BottomLocal true entity world
                                let world = World.publishEntityChange (nameof entityState.BottomLeftLocal) bottomLeftPrevious entityState.BottomLeftLocal true entity world
                                let world = World.publishEntityChange (nameof entityState.MinLocal) minPrevious entityState.MinLocal true entity world
                                let world = World.publishEntityChange (nameof entityState.MaxLocal) maxPrevious entityState.MaxLocal true entity world
                                struct (entityState, world)
                            else struct (entityState, world)
                        else struct (entityState, world)

                    // compute position
                    let position =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let affineMatrix = World.getEntityAffineMatrix mount world
                            Vector3.Transform (value, affineMatrix)
                        | _ -> value

                    // update property
                    let world = World.setEntityPosition position entity world |> snd'
                    struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal setEntityCenterLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.CenterLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityBottomLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.BottomLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityBottomLeftLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.BottomLeftLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityMinLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.MinLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityMaxLocal value entity world =
            let entityState = World.getEntityState entity world
            let offset = entityState.PositionLocal - entityState.MaxLocal
            World.setEntityPositionLocal (value + offset) entity world

        static member internal setEntityRotation value entity world =
            let entityState = World.getEntityState entity world
            if quatNeq value entityState.Rotation then
                if entityState.Optimized then
                    entityState.Rotation <- value
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Rotation <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityRotationLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if quatNeq value entityState.RotationLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                let anglesLocal = value.RollPitchYaw
                if entityState.Optimized then
                    entityState.RotationLocal <- value
                    entityState.AnglesLocal <- anglesLocal
                    let rotation =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationLocal = World.getEntityRotation mount world
                            rotationLocal * value
                        | _ -> value
                    entityState.Rotation <- rotation
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)

                else // do updates and propagation out-of-place

                    // update RotationLocal property
                    let struct (entityState, world) =
                        let previous = entityState.RotationLocal
                        let previousAnglesLocal = entityState.AnglesLocal
                        let previousDegreesLocal = entityState.DegreesLocal
                        if quatNeq value previous then
                            let struct (entityState, world) =
                                if entityState.Imperative then
                                    entityState.RotationLocal <- value
                                    entityState.AnglesLocal <- anglesLocal
                                    struct (entityState, world)
                                else
                                    let entityState = { entityState with RotationLocal = value }
                                    struct (entityState, World.setEntityState entityState entity world)
                            let publishChangeEvents = entityState.PublishChangeEvents
                            let world = World.publishEntityChange (nameof entityState.RotationLocal) previous value publishChangeEvents entity world
                            let world = World.publishEntityChange (nameof entityState.AnglesLocal) previousAnglesLocal anglesLocal publishChangeEvents entity world
                            let world = World.publishEntityChange (nameof entityState.DegreesLocal) previousDegreesLocal (Math.RadiansToDegrees3d anglesLocal) publishChangeEvents entity world
                            struct (entityState, world)
                        else struct (entityState, world)

                    // compute rotation
                    let rotation =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationMount = World.getEntityRotation mount world
                            rotationMount * value
                        | _ -> value

                    // update property
                    let world = World.setEntityRotation rotation entity world |> snd'
                    struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal setEntityScale value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Scale then
                if entityState.Optimized then
                    entityState.Scale <- value
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Scale <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityScaleLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.ScaleLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.ScaleLocal <- value
                    let scale =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let scale = World.getEntityScale mount world
                            value * scale
                        | _ -> value
                    entityState.Scale <- scale
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)

                else // do updates and propagation out-of-place

                    // update ScaleLocal property
                    let struct (entityState, world) =
                        let previous = entityState.ScaleLocal
                        if v3Neq value previous then
                            let struct (entityState, world) =
                                if entityState.Imperative then
                                    entityState.ScaleLocal <- value
                                    struct (entityState, world)
                                else
                                    let entityState = { entityState with ScaleLocal = value }
                                    struct (entityState, World.setEntityState entityState entity world)
                            let world = World.publishEntityChange (nameof entityState.ScaleLocal) previous value entityState.PublishChangeEvents entity world
                            struct (entityState, world)
                        else struct (entityState, world)

                    // compute scale
                    let scale =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let scale = World.getEntityScale mount world
                            value * scale
                        | _ -> value

                    // update property
                    let world = World.setEntityScale scale entity world |> snd'
                    struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal setEntityOffset value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Offset then
                if entityState.Optimized then
                    entityState.Offset <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Offset <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntitySize value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Size then
                let centerPrevious = entityState.CenterLocal
                let bottomPrevious = entityState.BottomLocal
                let bottomLeftPrevious = entityState.BottomLeftLocal
                let minPrevious = entityState.MinLocal
                let maxPrevious = entityState.MaxLocal
                if entityState.Optimized then
                    entityState.Size <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Size <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    let world =
                        if entityState.PublishChangeEvents then
                            let world = World.publishEntityChange (nameof entityState.CenterLocal) centerPrevious entityState.CenterLocal true entity world
                            let world = World.publishEntityChange (nameof entityState.BottomLocal) bottomPrevious entityState.BottomLocal true entity world
                            let world = World.publishEntityChange (nameof entityState.BottomLeftLocal) bottomLeftPrevious entityState.BottomLeftLocal true entity world
                            let world = World.publishEntityChange (nameof entityState.MinLocal) minPrevious entityState.MinLocal true entity world
                            let world = World.publishEntityChange (nameof entityState.MaxLocal) maxPrevious entityState.MaxLocal true entity world
                            world
                        else world
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityAngles value entity world =
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.Angles then
                if entityState.Optimized then
                    entityState.Angles <- value
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Angles <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityAnglesLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if v3Neq value entityState.AnglesLocal then

                // OPTIMIZATION: do updates and propagation in-place as much as possible.
                let rotationLocal = value.RollPitchYaw
                if entityState.Optimized then
                    entityState.RotationLocal <- rotationLocal
                    entityState.AnglesLocal <- value
                    let rotation =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world ->
                            let rotationMount = World.getEntityRotation mount world
                            rotationMount * rotationLocal
                        | _ -> rotationLocal
                    entityState.Rotation <- rotation
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)

                else // do updates and propagation out-of-place

                    // update AnglesLocal property
                    let struct (entityState, world) =
                        let previous = entityState.AnglesLocal
                        let previousRotationLocal = entityState.RotationLocal
                        let previousDegreesLocal = entityState.DegreesLocal
                        if v3Neq value previous then
                            let struct (entityState, world) =
                                if entityState.Imperative then
                                    entityState.RotationLocal <- rotationLocal
                                    entityState.AnglesLocal <- value
                                    struct (entityState, world)
                                else
                                    let entityState = { entityState with RotationLocal = rotationLocal; AnglesLocal = value }
                                    struct (entityState, World.setEntityState entityState entity world)
                            let publishChangeEvents = entityState.PublishChangeEvents
                            let world = World.publishEntityChange (nameof entityState.RotationLocal) previousRotationLocal rotationLocal publishChangeEvents entity world
                            let world = World.publishEntityChange (nameof entityState.AnglesLocal) previous value publishChangeEvents entity world
                            let world = World.publishEntityChange (nameof entityState.DegreesLocal) previousDegreesLocal (Math.RadiansToDegrees3d value) publishChangeEvents entity world
                            struct (entityState, world)
                        else struct (entityState, world)

                    // update rotation property if mounting, otherwise update angles property
                    match Option.bind (tryResolve entity) entityState.MountOpt with
                    | Some mount when World.getEntityExists mount world ->
                        let rotationMount = World.getEntityRotation mount world
                        let rotation = rotationMount * rotationLocal
                        let world = World.setEntityRotation rotation entity world |> snd'
                        struct (true, world)
                    | _ ->
                        let world = World.setEntityAngles value entity world |> snd'
                        struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal setEntityDegrees value entity world =
            World.setEntityAngles (Math.DegreesToRadians3d value) entity world

        static member internal setEntityDegreesLocal value entity world =
            World.setEntityAnglesLocal (Math.DegreesToRadians3d value) entity world

        static member internal propagateEntityElevation3 mount mounter world =
            let elevationMount = World.getEntityElevation mount world
            let elevationLocal = World.getEntityElevationLocal mounter world
            let elevation = elevationMount + elevationLocal
            let world = World.setEntityElevation elevation mounter world |> snd'
            World.traverseEntityMounters World.propagateEntityElevation3 mounter world

        static member internal propagateEntityElevation entity world =
            World.traverseEntityMounters World.propagateEntityElevation3 entity world

        static member internal setEntityElevation value entity world =
            let entityState = World.getEntityState entity world
            if value <> entityState.Transform.Elevation then
                if entityState.Optimized then
                    entityState.Transform.Elevation <- value
                    let world = if entityState.Mounted then World.propagateEntityElevation entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Elevation <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityElevationLocal value entity world =

            // ensure value changed
            let entityState = World.getEntityState entity world
            if value <> entityState.ElevationLocal then

                // OPTIMIZATION: do elevation updates and propagation in-place as much as possible.
                if entityState.Optimized then
                    entityState.ElevationLocal <- value
                    let elevationMount =
                        match Option.bind (tryResolve entity) entityState.MountOpt with
                        | Some mount when World.getEntityExists mount world -> World.getEntityElevation mount world
                        | _ -> 0.0f
                    entityState.Transform.Elevation <- elevationMount + value
                    let world = if entityState.Mounted then World.propagateEntityElevation entity world else world
                    struct (true, world)

                else // do elevation updates and propagation out-of-place

                    // update ElevationLocal property
                    let world =
                        let previous = entityState.ElevationLocal
                        if value <> previous then
                            let struct (entityState, world) =
                                if entityState.Imperative then
                                    entityState.ElevationLocal <- value
                                    struct (entityState, world)
                                else
                                    let entityState = { entityState with ElevationLocal = value }
                                    struct (entityState, World.setEntityState entityState entity world)
                            World.publishEntityChange (nameof entityState.ElevationLocal) previous value entityState.PublishChangeEvents entity world
                        else world

                    // compute mount elevation
                    let elevationMount =
                        match Option.bind (tryResolve entity) (World.getEntityMountOpt entity world) with
                        | Some mount when World.getEntityExists mount world -> World.getEntityElevation mount world
                        | _ -> 0.0f

                    // update property
                    let world = World.setEntityElevation (elevationMount + value) entity world |> snd'
                    struct (true, world)

            // nothing changed
            else struct (false, world)

        static member internal propagateEntityEnabled3 mount mounter world =
            let enabledMount = World.getEntityEnabled mount world
            let enabledLocal = World.getEntityEnabledLocal mounter world
            let enabled = enabledMount && enabledLocal
            let world = World.setEntityEnabled enabled mounter world |> snd'
            World.traverseEntityMounters World.propagateEntityEnabled3 mounter world

        static member internal propagateEntityEnabled entity world =
            World.traverseEntityMounters World.propagateEntityEnabled3 entity world

        static member internal setEntityEnabled value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Enabled
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Enabled <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.Enabled <- value
                        let world = World.setEntityState entityState entity world
                        struct (entityState, world)
                let world = World.publishEntityChange (nameof entityState.Enabled) previous value entityState.PublishChangeEvents entity world
                let world = if World.getEntityMounted entity world then World.propagateEntityEnabled entity world else world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityEnabledLocal value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.EnabledLocal
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.EnabledLocal <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.EnabledLocal <- value
                        let world = World.setEntityState entityState entity world
                        struct (entityState, world)
                let world = World.publishEntityChange (nameof entityState.EnabledLocal) previous value entityState.PublishChangeEvents entity world
                let mountOpt = Option.bind (tryResolve entity) (World.getEntityMountOpt entity world)
                let enabledMount =
                    match mountOpt with
                    | Some mount when World.getEntityExists mount world -> World.getEntityEnabled mount world
                    | _ -> true
                let enabled = enabledMount && value
                let world = World.setEntityEnabled enabled entity world |> snd'
                struct (true, world)
            else struct (false, world)

        static member internal propagateEntityVisible3 mount mounter world =
            let visibleMount = World.getEntityVisible mount world
            let visibleLocal = World.getEntityVisibleLocal mounter world
            let visible = visibleMount && visibleLocal
            let world = World.setEntityVisible visible mounter world |> snd'
            World.traverseEntityMounters World.propagateEntityVisible3 mounter world

        static member internal propagateEntityVisible entity world =
            World.traverseEntityMounters World.propagateEntityVisible3 entity world
            
        static member internal setEntityVisible value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Visible
            if value <> previous then
                let worldOld = world
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityState.Static
                let lightProbeOld = entityState.LightProbe
                let lightOld = entityState.Light
                let presenceOld = entityState.Presence
                let boundsOld = entityState.Bounds
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.Visible <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.Visible <- value
                        let world = World.setEntityState entityState entity world
                        struct (entityState, world)
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChange (nameof entityState.Visible) previous value entityState.PublishChangeEvents entity world
                let world = if World.getEntityMounted entity world then World.propagateEntityVisible entity world else world
                struct (true, world)
            else struct (false, world)

        static member internal setEntityVisibleLocal value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.VisibleLocal
            if value <> previous then
                let struct (entityState, world) =
                    if entityState.Imperative then
                        entityState.VisibleLocal <- value
                        struct (entityState, world)
                    else
                        let entityState = EntityState.diverge entityState
                        entityState.VisibleLocal <- value
                        let world = World.setEntityState entityState entity world
                        struct (entityState, world)
                let world = World.publishEntityChange (nameof entityState.VisibleLocal) previous value entityState.PublishChangeEvents entity world
                let mountOpt = Option.bind (tryResolve entity) (World.getEntityMountOpt entity world)
                let enabledMount =
                    match mountOpt with
                    | Some mount when World.getEntityExists mount world -> World.getEntityVisible mount world
                    | _ -> true
                let enabled = enabledMount && value
                let world = World.setEntityVisible enabled entity world |> snd'
                struct (true, world)
            else struct (false, world)

        static member internal setEntityPickable value entity world =
            let entityState = World.getEntityState entity world
            let previous = entityState.Pickable
            if value <> previous then
                if entityState.Imperative then
                    entityState.Pickable <- value
                    struct (true, world)
                else
                    let entityState = EntityState.diverge entityState
                    entityState.Pickable <- value
                    struct (true, World.setEntityState entityState entity world)
            else struct (false, world)

        static member internal setEntityOverflow value entity world =
            let entityState = World.getEntityState entity world
            if value <> entityState.Transform.Overflow then
                if entityState.Optimized then
                    entityState.Transform.Overflow <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Overflow <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal setEntityCentered value entity world =
            let entityState = World.getEntityState entity world
            if value <> entityState.Centered then
                if entityState.Optimized then
                    entityState.Centered <- value
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Centered <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal getEntityPerimeterUnscaled entity world =
            (World.getEntityState entity world).Transform.PerimeterUnscaled

        static member internal setEntityPerimeterUnscaled value entity world =
            let entityState = World.getEntityState entity world
            if box3Neq value entityState.PerimeterUnscaled then
                if entityState.Optimized then
                    entityState.PerimeterUnscaled <- value
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.PerimeterUnscaled <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal getEntityPerimeter entity world =
            (World.getEntityState entity world).Transform.Perimeter

        static member internal setEntityPerimeter value entity world =
            let entityState = World.getEntityState entity world
            if box3Neq value entityState.Perimeter then
                if entityState.Optimized then
                    entityState.Perimeter <- value
                    let world = if entityState.Mounted then World.propagateEntityAffineMatrix entity world else world
                    struct (true, world)
                else
                    let mutable transform = entityState.Transform
                    transform.Perimeter <- value
                    let world = World.setEntityTransformByRef (&transform, entityState, entity, world) |> snd'
                    struct (true, world)
            else struct (false, world)

        static member internal getEntityPerimeterOriented entity world =
            (World.getEntityState entity world).Transform.PerimeterOriented

        static member internal getEntityBounds entity world =
            (World.getEntityState entity world).Transform.Bounds

        static member private tryGetFacet facetName world =
            let facets = World.getFacets world
            match Map.tryFind facetName facets with
            | Some facet -> Right facet
            | None -> Left ("Invalid facet name '" + facetName + "'.")

        static member private isFacetCompatibleWithEntity entityDispatcherMap facet (entityState : EntityState) =
            // Note a facet is incompatible with any other facet if it contains any properties that has
            // the same name but a different type.
            let facetType = facet.GetType ()
            let facetPropertyDefinitions = Reflection.getPropertyDefinitions facetType
            if Reflection.isFacetCompatibleWithDispatcher entityDispatcherMap facet entityState then
                List.notExists
                    (fun (propertyDefinition : PropertyDefinition) ->
                        let mutable property = Unchecked.defaultof<_>
                        match Xtension.tryGetProperty (propertyDefinition.PropertyName, entityState.Xtension, &property) with
                        | true -> property.PropertyType <> propertyDefinition.PropertyType
                        | false -> false)
                    facetPropertyDefinitions
            else false

        static member private getEntityPropertyDefinitionNamesToDetach entityState facetToRemove =

            // get the property definition name counts of the current, complete entity
            let propertyDefinitions = Reflection.getReflectivePropertyDefinitionMap entityState
            let propertyDefinitionNameCounts = Reflection.getPropertyNameCounts propertyDefinitions

            // get the property definition name counts of the facet to remove
            let facetType = facetToRemove.GetType ()
            let facetPropertyDefinitions = Map.singleton facetType.Name (Reflection.getPropertyDefinitions facetType)
            let facetPropertyDefinitionNameCounts = Reflection.getPropertyNameCounts facetPropertyDefinitions

            // compute the difference of the counts
            let finalPropertyDefinitionNameCounts =
                Map.map
                    (fun propertyName propertyCount ->
                        match Map.tryFind propertyName facetPropertyDefinitionNameCounts with
                        | Some facetPropertyCount -> propertyCount - facetPropertyCount
                        | None -> propertyCount)
                    propertyDefinitionNameCounts

            // build a set of all property names where the final counts are negative
            Map.fold
                (fun propertyNamesToDetach propertyName propertyCount ->
                    if propertyCount = 0
                    then Set.add propertyName propertyNamesToDetach
                    else propertyNamesToDetach)
                Set.empty
                finalPropertyDefinitionNameCounts

        /// Get an entity's intrinsic facet names.
        static member getEntityIntrinsicFacetNames entityState =
            let intrinsicFacetNames = entityState.Dispatcher |> getType |> Reflection.getIntrinsicFacetNames
            Set.ofList intrinsicFacetNames

        /// Get an entity's facet names via reflection.
        static member getEntityFacetNamesReflectively (entityState : EntityState) =
            let facetNames = Array.map getTypeName entityState.Facets
            Set.ofArray facetNames

        static member private tryRemoveFacet facetName (entityState : EntityState) entityOpt world =
            match Array.tryFind (fun facet -> getTypeName facet = facetName) entityState.Facets with
            | Some facet ->
                let struct (entityState, world) =
                    match entityOpt with
                    | Some entity ->
                        let world = World.setEntityState entityState entity world
                        let world = facet.Register (entity, world)
                        let world =
                            if WorldModule.getSelected entity world
                            then facet.UnregisterPhysics (entity, world)
                            else world
                        let entityState = World.getEntityState entity world
                        struct (entityState, world)
                    | None -> struct (entityState, world)
                let propertyNames = World.getEntityPropertyDefinitionNamesToDetach entityState facet
                let entityState = Reflection.detachPropertiesViaNames EntityState.diverge propertyNames entityState
                let entityState =
                    let facetNames = Set.remove facetName entityState.FacetNames
                    let facets = Array.remove ((=) facet) entityState.Facets
                    let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                    entityState.FacetNames <- facetNames
                    entityState.Facets <- facets
                    entityState
                match entityOpt with
                | Some entity ->
                    let worldOld = world
                    let entityStateOld = entityState
                    let visibleOld = entityState.Visible || entityState.AlwaysRender
                    let staticOld = entityStateOld.Static
                    let lightProbeOld = entityStateOld.LightProbe
                    let lightOld = entityStateOld.Light
                    let presenceOld = entityStateOld.Presence
                    let boundsOld = entityStateOld.Bounds
                    let world = World.setEntityState entityState entity world
                    let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                    Right (World.getEntityState entity world, world)
                | None -> Right (entityState, world)
            | None -> Left ("Failure to remove facet '" + facetName + "' from entity.")

        static member private tryAddFacet facetName (entityState : EntityState) entityOpt world =
            match World.tryGetFacet facetName world with
            | Right facet ->
                let entityDispatchers = World.getEntityDispatchers world
                if World.isFacetCompatibleWithEntity entityDispatchers facet entityState then
                    let entityState =
                        let facetNames = Set.add facetName entityState.FacetNames
                        let facets = Array.add facet entityState.Facets
                        let entityState = if entityState.Imperative then entityState else EntityState.diverge entityState
                        entityState.FacetNames <- facetNames
                        entityState.Facets <- facets
                        entityState
                    let entityState = Reflection.attachProperties EntityState.diverge facet entityState world
                    match entityOpt with
                    | Some entity ->
                        let worldOld = world
                        let entityStateOld = entityState
                        let visibleOld = entityState.Visible || entityState.AlwaysRender
                        let staticOld = entityStateOld.Static
                        let lightProbeOld = entityStateOld.LightProbe
                        let lightOld = entityStateOld.Light
                        let presenceOld = entityStateOld.Presence
                        let boundsOld = entityStateOld.Bounds
                        let world = World.setEntityState entityState entity world
                        let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                        let world = facet.Register (entity, world)
                        let world =
                            if WorldModule.getSelected entity world
                            then facet.RegisterPhysics (entity, world)
                            else world
                        Right (World.getEntityState entity world, world)
                    | None -> Right (entityState, world)
                else Left ("Facet '" + getTypeName facet + "' is incompatible with entity '" + scstring entityState.Surnames + "'.")
            | Left error -> Left error

        static member private tryRemoveFacets facetNamesToRemove entityState entityOpt world =
            Set.fold
                (fun eitherEntityWorld facetName ->
                    match eitherEntityWorld with
                    | Right (entityState, world) -> World.tryRemoveFacet facetName entityState entityOpt world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToRemove

        static member private tryAddFacets facetNamesToAdd entityState entityOpt world =
            Set.fold
                (fun eitherEntityStateWorld facetName ->
                    match eitherEntityStateWorld with
                    | Right (entityState, world) -> World.tryAddFacet facetName entityState entityOpt world
                    | Left _ as left -> left)
                (Right (entityState, world))
                facetNamesToAdd

        static member private updateEntityPublishEventFlag setFlag entity eventAddress world =
            let publishEvent =
                match UMap.tryFind eventAddress (World.getSubscriptions world) with
                | Some subscriptions ->
                    if OMap.isEmpty subscriptions
                    then failwithumf () // NOTE: event system is defined to clean up all empty subscription entries
                    else true
                | None -> false
            if World.getEntityExists entity world
            then setFlag publishEvent entity world
            else struct (false, world)

        static member internal trySetFacetNames facetNames entityState entityOpt world =
            let intrinsicFacetNames = World.getEntityIntrinsicFacetNames entityState
            let extrinsicFacetNames = Set.fold (flip Set.remove) facetNames intrinsicFacetNames
            let facetNamesToRemove = Set.difference entityState.FacetNames extrinsicFacetNames
            let facetNamesToAdd = Set.difference extrinsicFacetNames entityState.FacetNames
            match World.tryRemoveFacets facetNamesToRemove entityState entityOpt world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState entityOpt world
            | Left _ as left -> left

        static member internal trySynchronizeFacetsToNames facetNamesOld entityState entityOpt world =
            let facetNamesToRemove = Set.difference facetNamesOld entityState.FacetNames
            let facetNamesToAdd = Set.difference entityState.FacetNames facetNamesOld
            match World.tryRemoveFacets facetNamesToRemove entityState entityOpt world with
            | Right (entityState, world) -> World.tryAddFacets facetNamesToAdd entityState entityOpt world
            | Left _ as left -> left

        static member internal attachIntrinsicFacetsViaNames entityState world =
            let entityDispatchers = World.getEntityDispatchers world
            let facets = World.getFacets world
            Reflection.attachIntrinsicFacets EntityState.diverge entityDispatchers facets entityState.Dispatcher entityState world

        static member internal applyEntityOverlay overlayerOld overlayer world entity =
            let entityState = World.getEntityState entity world
            match entityState.OverlayNameOpt with
            | Some overlayName ->
                let facetNamesOld = entityState.FacetNames
                let entityState = Overlayer.applyOverlayToFacetNames EntityState.diverge overlayName overlayName entityState overlayerOld overlayer
                match World.trySynchronizeFacetsToNames facetNamesOld entityState (Some entity) world with
                | Right (entityState, world) ->
                    let worldOld = world
                    let entityStateOld = entityState
                    let visibleOld = entityState.Visible || entityState.AlwaysRender
                    let staticOld = entityStateOld.Static
                    let lightProbeOld = entityStateOld.LightProbe
                    let lightOld = entityStateOld.Light
                    let presenceOld = entityStateOld.Presence
                    let boundsOld = entityStateOld.Bounds
                    let facetNames = World.getEntityFacetNamesReflectively entityState
                    let entityState = Overlayer.applyOverlay6 EntityState.diverge overlayName overlayName facetNames entityState overlayerOld overlayer
                    let world = World.setEntityState entityState entity world
                    World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                | Left error -> Log.info ("There was an issue in applying a reloaded overlay: " + error); world
            | None -> world

        static member internal tryGetEntityXtensionProperty (propertyName, entity, world, property : _ outref) =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> false
            | _ -> EntityState.tryGetProperty (propertyName, entityStateOpt, &property)

        static member internal tryGetEntityXtensionValue<'a> propertyName entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> failwithf "Could not find entity '%s'." (scstring entity)
            | _ ->
                let mutable property = Unchecked.defaultof<Property>
                if World.tryGetEntityProperty (propertyName, entity, world, &property) then
                    let valueObj =
                        match property.PropertyValue with
                        | :? DesignerProperty as dp -> dp.DesignerValue
                        | :? ComputedProperty as cp -> cp.ComputedGet (entity :> obj) (world :> obj)
                        | _ -> property.PropertyValue
                    match valueObj with
                    | :? 'a as value -> value
                    | null -> null :> obj :?> 'a
                    | value -> value |> valueToSymbol |> symbolToValue
                else Unchecked.defaultof<'a>

        static member internal tryGetEntityProperty (propertyName, entity, world, property : _ outref) =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> false
            | _ ->
                match EntityGetters.TryGetValue propertyName with
                | (true, getter) -> property <- getter entity world; true
                | (false, _) ->
                    if EntityState.tryGetProperty (propertyName, entityStateOpt, &property) then
                        if EntityState.containsRuntimeProperties entityStateOpt then
                            match property.PropertyValue with
                            | :? DesignerProperty as dp -> property <- { PropertyType = dp.DesignerType; PropertyValue = dp.DesignerValue }; true
                            | :? ComputedProperty as cp -> property <- { PropertyType = cp.ComputedType; PropertyValue = cp.ComputedGet (entity :> obj) (world :> obj) }; true
                            | _ -> true
                        else true
                    else false

        static member internal getEntityXtensionValue<'a> propertyName entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            match entityStateOpt :> obj with
            | null -> failwithf "Could not find entity '%s'." (scstring entity)
            | _ ->
                let property = EntityState.getProperty propertyName entityStateOpt
                let valueObj =
                    match property.PropertyValue with
                    | :? DesignerProperty as dp -> dp.DesignerValue
                    | :? ComputedProperty as cp -> cp.ComputedGet (entity :> obj) (world :> obj)
                    | _ -> property.PropertyValue
                match valueObj with
                | :? 'a as value -> value
                | null -> null :> obj :?> 'a
                | value -> value |> valueToSymbol |> symbolToValue

        static member internal getEntityProperty propertyName entity world =
            let mutable property = Unchecked.defaultof<_>
            match World.tryGetEntityProperty (propertyName, entity, world, &property) with
            | true -> property
            | false -> failwithf "Could not find property '%s'." propertyName

        static member internal trySetEntityXtensionPropertyWithoutEvent propertyName (property : Property) entityState entity world =
            let mutable propertyOld = Unchecked.defaultof<_>
            match EntityState.tryGetProperty (propertyName, entityState, &propertyOld) with
            | true ->
                if EntityState.containsRuntimeProperties entityState then
                    match propertyOld.PropertyValue with
                    | :? DesignerProperty as dp ->
                        let previous = dp.DesignerValue
                        if property.PropertyValue =/= previous then
                            let property = { property with PropertyValue = { dp with DesignerValue = property.PropertyValue }}
                            match EntityState.trySetProperty propertyName property entityState with
                            | struct (true, entityState) -> struct (true, true, previous, if entityState.Imperative then world else World.setEntityState entityState entity world)
                            | struct (false, _) -> struct (false, false, previous, world)
                        else (true, false, previous, world)
                    | :? ComputedProperty as cp ->
                        match cp.ComputedSetOpt with
                        | Some computedSet ->
                            let previous = cp.ComputedGet (box entity) (box world)
                            if property.PropertyValue =/= previous
                            then struct (true, true, previous, computedSet property.PropertyValue entity world :?> World)
                            else struct (true, false, previous, world)
                        | None -> struct (false, false, Unchecked.defaultof<_>, world)
                    | _ ->
                        let previous = propertyOld.PropertyValue
                        if property.PropertyValue =/= previous then
                            if entityState.Imperative then
                                // OPTIMIZATION: special-case for imperative
                                propertyOld.PropertyValue <- property.PropertyValue
                                struct (true, true, previous, world)
                            else
                                match EntityState.trySetProperty propertyName property entityState with
                                | struct (true, entityState) -> (true, true, previous, if entityState.Imperative then world else World.setEntityState entityState entity world)
                                | struct (false, _) -> struct (false, false, previous, world)
                        else struct (true, false, previous, world)
                else
                    let previous = propertyOld.PropertyValue
                    if CoreOperators.(=/=) property.PropertyValue previous then
                        if entityState.Imperative then
                            // OPTIMIZATION: special-case for imperative
                            propertyOld.PropertyValue <- property.PropertyValue
                            struct (true, true, previous, world)
                        else
                            match EntityState.trySetProperty propertyName property entityState with
                            | struct (true, entityState) -> (true, true, previous, if entityState.Imperative then world else World.setEntityState entityState entity world)
                            | struct (false, _) -> struct (false, false, previous, world)
                    else struct (true, false, previous, world)
            | false -> struct (false, false, Unchecked.defaultof<_>, world)

        static member internal trySetEntityXtensionPropertyFast propertyName property entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityStateOpt entity world with
                | struct (true, changed, previous, world) ->
                    if changed
                    then World.publishEntityChange propertyName previous property.PropertyValue entityStateOpt.PublishChangeEvents entity world
                    else world
                | struct (false, _, _, world) -> world
            else world

        static member internal trySetEntityXtensionProperty propertyName property entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityStateOpt entity world with
                | struct (true, changed, previous, world) ->
                    let world =
                        if changed
                        then World.publishEntityChange propertyName previous property.PropertyValue entityStateOpt.PublishChangeEvents entity world
                        else world
                    struct (true, changed, world)
                | struct (false, changed, _, world) -> struct (false, changed, world)
            else struct (false, false, world)

        static member internal setEntityXtensionPropertyWithoutEvent propertyName property entity world =
            let entityState = World.getEntityState entity world
            match World.trySetEntityXtensionPropertyWithoutEvent propertyName property entityState entity world with
            | struct (true, changed, _, world) -> struct (true, changed, world)
            | struct (false, _, _, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal setEntityXtensionValue<'a> propertyName (value : 'a) entity world =
            let entityStateOpt = World.getEntityStateOpt entity world
            if notNull (entityStateOpt :> obj) then
                let entityState = entityStateOpt
                let propertyOld = EntityState.getProperty propertyName entityState
                let mutable previous = Unchecked.defaultof<obj> // OPTIMIZATION: avoid passing around structs.
                let mutable changed = false // OPTIMIZATION: avoid passing around structs.
                let world =
                    if EntityState.containsRuntimeProperties entityState then
                        match propertyOld.PropertyValue with
                        | :? DesignerProperty as dp ->
                            previous <- dp.DesignerValue
                            if value =/= previous then
                                changed <- true
                                let property = { propertyOld with PropertyValue = { dp with DesignerValue = value }}
                                let entityState = EntityState.setProperty propertyName property entityState
                                if entityState.Imperative then world else World.setEntityState entityState entity world
                            else world
                        | :? ComputedProperty as cp ->
                            match cp.ComputedSetOpt with
                            | Some computedSet ->
                                previous <- cp.ComputedGet (box entity) (box world)
                                if value =/= previous then
                                    changed <- true
                                    computedSet propertyOld.PropertyValue entity world :?> World
                                else world
                            | None -> world
                        | _ ->
                            previous <- propertyOld.PropertyValue
                            if value =/= previous then
                                changed <- true
                                if entityState.Imperative then
                                    // OPTIMIZATION: special-case for imperative
                                    propertyOld.PropertyValue <- value
                                    world
                                else
                                    let property = { propertyOld with PropertyValue = value }
                                    let entityState = EntityState.setProperty propertyName property entityState
                                    if entityState.Imperative then world else World.setEntityState entityState entity world
                            else world
                    else
                        previous <- propertyOld.PropertyValue
                        if value =/= previous then
                            changed <- true
                            if entityState.Imperative then
                                // OPTIMIZATION: special-case for imperative
                                propertyOld.PropertyValue <- value
                                world
                            else
                                let property = { propertyOld with PropertyValue = value }
                                let entityState = EntityState.setProperty propertyName property entityState
                                if entityState.Imperative then world else World.setEntityState entityState entity world
                        else world
                if changed
                then World.publishEntityChange propertyName previous value entityStateOpt.PublishChangeEvents entity world
                else world
            else failwithf "Could not find entity '%s'." (scstring entity)

        static member internal setEntityXtensionProperty propertyName property entity world =
            match World.trySetEntityXtensionProperty propertyName property entity world with
            | struct (true, changed, world) -> struct (changed, world)
            | struct (false, _, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal trySetEntityPropertyFast propertyName property entity world =
            match EntitySetters.TryGetValue propertyName with
            | (true, setter) ->
                if World.getEntityExists entity world
                then setter property entity world |> snd'
                else world
            | (false, _) -> World.trySetEntityXtensionPropertyFast propertyName property entity world

        static member internal trySetEntityProperty propertyName property entity world =
            match EntitySetters.TryGetValue propertyName with
            | (true, setter) ->
                if World.getEntityExists entity world then
                    let struct (changed, world) = setter property entity world
                    struct (true, changed, world)
                else (false, false, world)
            | (false, _) -> World.trySetEntityXtensionProperty propertyName property entity world

        static member internal setEntityPropertyFast propertyName property entity world =
            match EntitySetters.TryGetValue propertyName with
            | (true, setter) -> setter property entity world |> snd'
            | (false, _) -> World.trySetEntityXtensionPropertyFast propertyName property entity world

        static member internal setEntityProperty propertyName property entity world =
            match World.trySetEntityProperty propertyName property entity world with
            | struct (true, changed, world) -> struct (changed, world)
            | struct (false, _, _) -> failwithf "Could not find property '%s'." propertyName

        static member internal attachEntityProperty propertyName property entity world =
            if World.getEntityExists entity world then
                let entityState = World.getEntityState entity world
                let entityState = EntityState.attachProperty propertyName property entityState
                let world = World.setEntityState entityState entity world
                World.publishEntityChange propertyName property.PropertyValue property.PropertyValue entityState.PublishChangeEvents entity world
            else failwith ("Cannot attach entity property '" + propertyName + "'; entity '" + scstring entity + "' is not found.")

        static member internal detachEntityProperty propertyName entity world =
            if World.getEntityExists entity world then
                let entityState = World.getEntityState entity world
                let entityState = EntityState.detachProperty propertyName entityState
                World.setEntityState entityState entity world
            else failwith ("Cannot detach entity property '" + propertyName + "'; entity '" + scstring entity + "' is not found.")

        static member internal getEntityDefaultOverlayName dispatcherName world =
            match World.tryGetRoutedOverlayNameOpt dispatcherName world with
            | Some _ as opt -> opt
            | None -> Some dispatcherName

        static member internal getEntityInView2dAbsolute entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.OmnipresentType || World.boundsInView2dAbsolute transform.Bounds.Box2 world

        static member internal getEntityInView2dRelative entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.OmnipresentType || World.boundsInView2dRelative transform.Bounds.Box2 world

        static member internal getEntityInPlay2dAbsolute entity world =
            World.getEntityInView2dAbsolute entity world

        static member internal getEntityInPlay2dRelative entity world =
            World.getEntityInView2dRelative entity world

        static member internal getEntityInPlay3d entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.OmnipresentType || World.boundsInPlay3d transform.Bounds world

        static member internal getEntityInView3d entity world =
            let entityState = World.getEntityState entity world
            let mutable transform = &entityState.Transform
            let presence = transform.Presence
            presence.OmnipresentType || World.boundsInView3d transform.LightProbe transform.Light presence transform.Bounds world

        static member internal getEntityQuickSize (entity : Entity) world =
            let dispatcher = World.getEntityDispatcher entity world
            let facets = World.getEntityFacets entity world
            let quickSize = dispatcher.GetQuickSize (entity, world)
            Array.fold
                (fun (maxSize : Vector3) (facet : Facet) ->
                    let quickSize = facet.GetQuickSize (entity, world)
                    Vector3
                        (Math.Max (quickSize.X, maxSize.X),
                         Math.Max (quickSize.Y, maxSize.Y),
                         Math.Max (quickSize.Z, maxSize.Z)))
                quickSize
                facets

        static member internal getEntitySortingPriority2d entity world =
            let entityState = World.getEntityState entity world
            { SortElevation = entityState.Transform.Elevation
              SortHorizon = entityState.Transform.Horizon
              SortTarget = entity }

        static member internal rayCastEntity ray (entity : Entity) world =
            let facets = World.getEntityFacets entity world
            let dispatcher = World.getEntityDispatcher entity world
            let intersectionsFacets = facets |> Array.map (fun facet -> facet.RayCast (ray, entity, world)) |> Array.concat
            let intersectionsDispatcher = dispatcher.RayCast (ray, entity, world)
            let intersections = Array.append intersectionsFacets intersectionsDispatcher
            Array.sort intersections

        static member internal getEntityHighlightBounds (entity : Entity) world =
            let mutable boundsOpt = None : Box3 option
            let facets = World.getEntityFacets entity world
            let dispatcher = World.getEntityDispatcher entity world
            for facet in facets do
                let bounds2Opt = facet.TryGetHighlightBounds (entity, world)
                match (boundsOpt, bounds2Opt) with
                | (Some bounds, Some bounds2) -> boundsOpt <- Some (bounds.Combine bounds2)
                | (Some _, None) -> ()
                | (None, Some _) -> boundsOpt <- bounds2Opt
                | (None, None) -> ()
            let bounds2Opt = dispatcher.TryGetHighlightBounds (entity, world)
            match (boundsOpt, bounds2Opt) with
            | (Some bounds, Some bounds2) -> boundsOpt <- Some (bounds.Combine bounds2)
            | (Some _, None) -> ()
            | (None, Some _) -> boundsOpt <- bounds2Opt
            | (None, None) -> ()
            match boundsOpt with
            | Some bounds -> bounds
            | None -> World.getEntityBounds entity world

#if !DISABLE_ENTITY_PRE_UPDATE
        static member internal updateEntityPublishPreUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishPreUpdates entity (atooa (entity.PreUpdateEvent --> entity)) world
#endif

        static member internal updateEntityPublishUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishUpdates entity (atooa (Events.UpdateEvent --> entity)) world

#if !DISABLE_ENTITY_POST_UPDATE
        static member internal updateEntityPublishPostUpdateFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishPostUpdates entity (atooa (Events.PostUpdateEvent --> entity)) world
#endif

        static member internal updateEntityPublishRenderFlag entity world =
            World.updateEntityPublishEventFlag World.setEntityPublishRenders entity (atooa (Events.RenderEvent --> entity)) world

        static member internal updateEntityPublishFlags entity world =
            let mutable changed = false // bit of funky mutation in the face of #if
#if !DISABLE_ENTITY_PRE_UPDATE
            let struct (changed', world) = World.updateEntityPublishPreUpdateFlag entity world
            changed <- changed || changed'
#endif
            let struct (changed', world) = World.updateEntityPublishUpdateFlag entity world
            changed <- changed || changed'
#if !DISABLE_ENTITY_POST_UPDATE
            let struct (changed', world) = World.updateEntityPublishPostUpdateFlag entity world
            changed <- changed || changed'
#endif
            let struct (changed', world) = World.updateEntityPublishRenderFlag entity world
            changed <- changed || changed'
            struct (changed, world)

        static member internal divergeEntity entity world =
            let entityState = World.getEntityState entity world
            let entityState = EntityState.diverge entityState
            World.setEntityState entityState entity world

        static member internal registerEntity entity world =
            let dispatcher = World.getEntityDispatcher entity world : EntityDispatcher
            let facets = World.getEntityFacets entity world
            let world = dispatcher.Register (entity, world)
            let world =
                Array.fold (fun world (facet : Facet) ->
                    let world = facet.Register (entity, world)
                    if WorldModule.getSelected entity world
                    then facet.RegisterPhysics (entity, world)
                    else world)
                    world facets
            let struct (_, world) = World.updateEntityPublishFlags entity world
            let eventTrace = EventTrace.debug "World" "registerEntity" "Register" EventTrace.empty
            let eventAddresses = EventGraph.getEventAddresses1 (Events.RegisterEvent --> entity)
            let world = Array.fold (fun world eventAddress -> World.publishPlus () eventAddress eventTrace entity false false world) world eventAddresses
            let eventTrace = EventTrace.debug "World" "registerEntity" "LifeCycle" EventTrace.empty
            let world = World.publishPlus (RegisterData entity) (Events.LifeCycleEvent (nameof Entity) --> Nu.Game.Handle) eventTrace entity false false world
            world

        static member internal unregisterEntity (entity : Entity) world =
            let eventTrace = EventTrace.debug "World" "unregisterEntity" "LifeCycle" EventTrace.empty
            let world = World.publishPlus (UnregisteringData entity) (Events.LifeCycleEvent (nameof Entity) --> Nu.Game.Handle) eventTrace entity false false world
            let eventTrace = EventTrace.debug "World" "unregister" "Unregistering" EventTrace.empty
            let eventAddresses = EventGraph.getEventAddresses1 (Events.UnregisteringEvent --> entity)
            let world = Array.fold (fun world eventAddress -> World.publishPlus () eventAddress eventTrace entity false false world) world eventAddresses
            let dispatcher = World.getEntityDispatcher entity world : EntityDispatcher
            let facets = World.getEntityFacets entity world
            let world = dispatcher.Unregister (entity, world)
            Array.fold (fun world (facet : Facet) ->
                let world = facet.Unregister (entity, world)
                if WorldModule.getSelected entity world
                then facet.UnregisterPhysics (entity, world)
                else world)
                world facets

        static member internal registerEntityPhysics entity world =
            let facets = World.getEntityFacets entity world
            Array.fold (fun world (facet : Facet) -> facet.RegisterPhysics (entity, world)) world facets

        static member internal unregisterEntityPhysics entity world =
            let facets = World.getEntityFacets entity world
            Array.fold (fun world (facet : Facet) -> facet.UnregisterPhysics (entity, world)) world facets

        static member internal propagateEntityPhysics entity world =
            let world = World.unregisterEntityPhysics entity world
            let world = World.registerEntityPhysics entity world
            world

        static member internal addEntity mayReplace entityState entity world =

            // add entity only if it is new or is explicitly able to be replaced
            let isNew = not (World.getEntityExists entity world)
            if isNew || mayReplace then

                // get old world for entity tree rebuild and change events
                let worldOld = world
                
                // add entity to world
                let world = World.addEntityState entityState entity world

                // update mount hierarchy
                let mountOpt = World.getEntityMountOpt entity world
                let world = World.addEntityToMounts mountOpt entity world

                // mutate respective spatial tree if entity is selected
                let world =
                    if WorldModule.getSelected entity world then
                        if World.getEntityIs2d entity world then
                            let quadtree =
                                MutantCache.mutateMutant
                                    (fun () -> worldOld.WorldExtension.Dispatchers.RebuildQuadtree worldOld)
                                    (fun entityTree ->
                                        let entityState = World.getEntityState entity world
                                        let element = Quadelement.make (entityState.Visible || entityState.AlwaysRender) entity
                                        Quadtree.addElement entityState.Presence entityState.Bounds.Box2 element entityTree
                                        entityTree)
                                    (World.getQuadtree world)
                            World.setQuadtree quadtree world
                        else
                            let octree =
                                MutantCache.mutateMutant
                                    (fun () -> worldOld.WorldExtension.Dispatchers.RebuildOctree worldOld)
                                    (fun entityTree ->
                                        let entityState = World.getEntityState entity world
                                        let element = Octelement.make (entityState.Visible || entityState.AlwaysRender) entityState.Static entityState.LightProbe entityState.Light entityState.Presence entityState.Bounds entity
                                        Octree.addElement entityState.Presence entityState.Bounds element entityTree
                                        entityTree)
                                    (World.getOctree world)
                            World.setOctree octree world
                    else world

                // register entity if needed
                if isNew
                then World.registerEntity entity world
                else world

            // handle failure
            else failwith ("Adding an entity that the world already contains '" + scstring entity + "'.")

        static member internal destroyEntityImmediateInternal recur entity world =

            // attempt to remove from destruction list
            let world = World.tryRemoveSimulantFromDestruction entity world

            // ensure entity exists in the world
            if World.getEntityExists entity world then

                // get old world for entity tree rebuild
                let worldOld = world

                // cache entity children for later possible destruction
                let children = World.getEntityChildren entity world

                // unmount from hierarchy
                let world = World.setEntityMountOpt None entity world |> snd'

                // unregister entity
                let world = World.unregisterEntity entity world

                // destroy any scheduled tasklets
                let world = World.removeTasklets entity world

                // mutate entity tree if entity is selected
                let world =
                    if WorldModule.getSelected entity world then
                        if World.getEntityIs2d entity world then
                            let quadtree =
                                MutantCache.mutateMutant
                                    (fun () -> world.WorldExtension.Dispatchers.RebuildQuadtree world)
                                    (fun quadtree ->
                                        let entityState = World.getEntityState entity worldOld
                                        let element = Quadelement.make (entityState.Visible || entityState.AlwaysRender) entity
                                        Quadtree.removeElement entityState.Presence entityState.Bounds.Box2 element quadtree
                                        quadtree)
                                    (World.getQuadtree world)
                            World.setQuadtree quadtree world
                        else
                            let octree =
                                MutantCache.mutateMutant
                                    (fun () -> world.WorldExtension.Dispatchers.RebuildOctree world)
                                    (fun octree ->
                                        let entityState = World.getEntityState entity worldOld
                                        let element = Octelement.make (entityState.Visible || entityState.AlwaysRender) entityState.Static entityState.LightProbe entityState.Light entityState.Presence entityState.Bounds entity
                                        Octree.removeElement entityState.Presence entityState.Bounds element octree
                                        octree)
                                    (World.getOctree world)
                            World.setOctree octree world
                    else world

                // remove cached entity event addresses
                EventGraph.cleanEventAddressCache entity.EntityAddress

                // invalidate entity state
                let entityState = World.getEntityState entity world
                entityState.Invalidated <- true

                // remove the entity from the world
                let world = World.removeEntityState entity world

                // destroy children when recurring
                if recur then
                    Seq.fold (fun world child ->
                        World.destroyEntityImmediateInternal recur child world)
                        world children
                else world

            // pass
            else world

        /// Destroy an entity in the world immediately. Can be dangerous if existing in-flight publishing depends on
        /// the entity's existence. Consider using World.destroyEntity instead.
        static member destroyEntityImmediate entity world =
            World.destroyEntityImmediateInternal true entity world

        /// Create an entity and add it to the world.
        static member createEntity5 dispatcherName overlayDescriptor surnames (group : Group) world =

            (* TODO: factor out common code between this and readEntity - there's just too much. *)

            // find the entity's dispatcher
            let dispatchers = World.getEntityDispatchers world
            let dispatcher =
                match Map.tryFind dispatcherName dispatchers with
                | Some dispatcher -> dispatcher
                | None -> failwith ("Could not find an EntityDispatcher named '" + dispatcherName + "'.")

            // compute the optional overlay name
            let overlayNameDefault = Overlay.dispatcherNameToOverlayName dispatcherName
            let overlayNameOpt =
                match overlayDescriptor with
                | NoOverlay -> None
                | RoutedOverlay -> World.tryGetRoutedOverlayNameOpt dispatcherName world
                | DefaultOverlay -> Some (Option.defaultValue overlayNameDefault (World.tryGetRoutedOverlayNameOpt dispatcherName world))
                | ExplicitOverlay overlayName -> Some overlayName

            // make the bare entity state (with name as id if none is provided)
            let entityState = EntityState.make (World.getImperative world) surnames overlayNameOpt dispatcher

            // attach the entity state's intrinsic facets and their properties
            let entityState = World.attachIntrinsicFacetsViaNames entityState world

            // apply the entity state's overlay to its facet names
            let overlayer = World.getOverlayer world
            let entityState =
                match overlayNameOpt with
                | Some overlayName ->

                    // apply overlay to facets
                    let entityState = Overlayer.applyOverlayToFacetNames id dispatcherName overlayName entityState overlayer overlayer

                    // synchronize the entity's facets (and attach their properties)
                    match World.trySynchronizeFacetsToNames Set.empty entityState None world with
                    | Right (entityState, _) -> entityState
                    | Left error -> Log.debug error; entityState
                | None -> entityState

            // attach the entity state's dispatcher properties
            let entityState = Reflection.attachProperties id entityState.Dispatcher entityState world

            // apply the entity state's overlay if exists
            let entityState =
                match entityState.OverlayNameOpt with
                | Some overlayName ->
                    // OPTIMIZATION: apply overlay only when it will change something
                    if overlayNameDefault <> overlayName then
                        let facetNames = World.getEntityFacetNamesReflectively entityState
                        Overlayer.applyOverlay id overlayNameDefault overlayName facetNames entityState overlayer
                    else entityState
                | None -> entityState

            // make entity address
            let entityAddress = group.GroupAddress <-- rtoa<Entity> entityState.Surnames

            // make entity reference
            let entity = Entity entityAddress

            // add entity's state to world
            let world =
                if World.getEntityExists entity world then
                    if World.getEntityDestroying entity world
                    then World.destroyEntityImmediate entity world
                    else failwith ("Entity '" + scstring entity + "' already exists and cannot be created."); world
                else world
            let world = World.addEntity false entityState entity world

            // update optimization flags
#if !DISABLE_ENTITY_PRE_UPDATE
            let world = World.updateEntityPublishPreUpdateFlag entity world |> snd'
#endif
            let world = World.updateEntityPublishUpdateFlag entity world |> snd'
#if !DISABLE_ENTITY_POST_UPDATE
            let world = World.updateEntityPublishPostUpdateFlag entity world |> snd'
#endif

            // update mount hierarchy
            let mountOpt = World.getEntityMountOpt entity world
            let world = World.addEntityToMounts mountOpt entity world

            // propagate properties
            let world =
                if World.getEntityMounted entity world then
                    let world = World.propagateEntityAffineMatrix entity world
                    let world = World.propagateEntityElevation entity world
                    let world = World.propagateEntityEnabled entity world
                    let world = World.propagateEntityVisible entity world
                    world
                else world

            // fin
            (entity, world)

        /// Create an entity from a simulant descriptor.
        static member createEntity4 overlayDescriptor descriptor group world =
            let (entity, world) =
                World.createEntity5 descriptor.SimulantDispatcherName overlayDescriptor descriptor.SimulantSurnamesOpt group world
            let world =
                List.fold (fun world (propertyName, property) ->
                    World.setEntityProperty propertyName property entity world |> snd')
                    world descriptor.SimulantProperties
            let world =
                if WorldModule.getSelected entity world
                then World.propagateEntityPhysics entity world
                else world
            (entity, world)

        /// Create an entity and add it to the world.
        static member createEntity<'d when 'd :> EntityDispatcher> overlayDescriptor surnamesOpt group world =
            World.createEntity5 typeof<'d>.Name overlayDescriptor surnamesOpt group world

        /// Duplicate an entity.
        static member duplicateEntity source (destination : Entity) world =
            let entityStateOpt = World.getEntityStateOpt source world
            match entityStateOpt :> obj with
            | null -> world
            | _ ->
                let entityState = { entityStateOpt with Order = Core.getTimeStampUnique (); Id = Gen.id64; Surnames = destination.Surnames }
                World.addEntity false entityState destination world

        /// Rename an entity. Note that since this destroys the renamed entity immediately, you should not call this
        /// inside an event handler that involves the reassigned entity itself. Note this also renames all of its
        /// descendents accordingly.
        static member renameEntityImmediate source (destination : Entity) world =
            let entityStateOpt = World.getEntityStateOpt source world
            match entityStateOpt :> obj with
            | null -> world
            | _ ->
                let entityState = { entityStateOpt with Id = Gen.id64; Surnames = destination.Surnames }
                let children = World.getEntityChildren source world
                let world = World.destroyEntityImmediateInternal false source world
                let world = World.addEntity false entityState destination world
                Seq.fold (fun world (child : Entity) ->
                    let destination = destination / child.Name
                    World.renameEntityImmediate child destination world)
                    world children

        /// Rename an entity.
        static member renameEntity source destination world =
            World.frame (World.renameEntityImmediate source destination) Game.Handle world

        /// Try to set an entity's optional overlay name.
        static member trySetEntityOverlayNameOpt overlayNameOpt entity world =
            let entityStateOld = World.getEntityState entity world
            let overlayerNameOldOpt = entityStateOld.OverlayNameOpt
            let entityState =
                if entityStateOld.Imperative then
                    entityStateOld.OverlayNameOpt <- overlayNameOpt
                    entityStateOld
                else { entityStateOld with OverlayNameOpt = overlayNameOpt }
            match (overlayerNameOldOpt, overlayNameOpt) with
            | (Some overlayerNameOld, Some overlayName) ->
                let overlayer = World.getOverlayer world
                let (entityState, world) =
                    let facetNamesOld = entityState.FacetNames
                    let entityState = Overlayer.applyOverlayToFacetNames EntityState.diverge overlayerNameOld overlayName entityState overlayer overlayer
                    match World.trySynchronizeFacetsToNames facetNamesOld entityState (Some entity) world with
                    | Right (entityState, world) -> (entityState, world)
                    | Left error -> Log.debug error; (entityState, world)
                let facetNames = World.getEntityFacetNamesReflectively entityState
                let entityState = Overlayer.applyOverlay EntityState.copy overlayerNameOld overlayName facetNames entityState overlayer
                let worldOld = world
                let entityStateOld = entityState
                let visibleOld = entityState.Visible || entityState.AlwaysRender
                let staticOld = entityStateOld.Static
                let lightProbeOld = entityStateOld.LightProbe
                let lightOld = entityStateOld.Light
                let presenceOld = entityStateOld.Presence
                let boundsOld = entityStateOld.Bounds
                let world = World.setEntityState entityState entity world
                let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                let world = World.publishEntityChanges entity world
                (Right (), world)
            | (None, None) ->
                (Right (), world)
            | (_, _) ->
                (Left "Could not set the entity's overlay name to None because doing so is currently not implemented.", world)
            
        /// Try to set the entity's facet names from script.
        static member internal trySetEntityOverlayNameOptFromScript overlayNameOpt entity world =
            match World.trySetEntityOverlayNameOpt overlayNameOpt entity world with
            | (Right _, world) -> world
            | (Left _, world) -> world

        /// Try to set the entity's facet names.
        static member trySetEntityFacetNames facetNames entity world =
            let entityState = World.getEntityState entity world
            let facetNamesOld = entityState.FacetNames
            if facetNames <> facetNamesOld then
                match World.trySetFacetNames facetNames entityState (Some entity) world with
                | Right (entityState, world) ->
                    let worldOld = world
                    let entityStateOld = entityState
                    let visibleOld = entityState.Visible || entityState.AlwaysRender
                    let staticOld = entityStateOld.Static
                    let lightProbeOld = entityStateOld.LightProbe
                    let lightOld = entityStateOld.Light
                    let presenceOld = entityStateOld.Presence
                    let boundsOld = entityStateOld.Bounds
                    let world = World.setEntityState entityState entity world
                    let world = World.updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld presenceOld boundsOld entity worldOld world
                    let world = World.publishEntityChange Constants.Engine.FacetNamesPropertyName facetNamesOld entityState.FacetNames true entity world
                    let world = World.publishEntityChanges entity world
                    (Right (), world)
                | Left error -> (Left error, world)
            else (Right (), world)

        /// Try to set the entity's facet names from script.
        static member internal trySetEntityFacetNamesFromScript facetNames entity world =
            match World.trySetEntityFacetNames facetNames entity world with
            | (Right _, world) -> world
            | (Left _, world) -> world

        static member internal updateEntityInEntityTree visibleOld staticOld lightProbeOld lightOld (presenceOld : Presence) boundsOld (entity : Entity) worldOld world =

            // only do this when entity is selected
            if WorldModule.getSelected entity world then

                // OPTIMIZATION: work with the entity state directly to avoid function call overheads
                let entityState = World.getEntityState entity world
                let visibleNew = entityState.Visible || entityState.AlwaysRender
                let staticNew = entityState.Static
                let lightProbeNew = entityState.LightProbe
                let lightNew = entityState.Light
                let presenceNew = entityState.Presence
                let boundsNew = entityState.Bounds

                // OPTIMIZATION: only update when relevant entity state has changed.
                if  visibleNew <> visibleOld ||
                    staticNew <> staticOld ||
                    lightProbeNew <> lightProbeOld ||
                    lightNew <> lightOld ||
                    presenceNeq presenceNew presenceOld ||
                    box3Neq boundsOld boundsNew then

                    // update entity in entity tree
                    if entityState.Is2d then
                        let quadree =
                            MutantCache.mutateMutant
                                (fun () -> worldOld.WorldExtension.Dispatchers.RebuildQuadtree worldOld)
                                (fun quadree ->
                                    let element = Quadelement.make visibleNew entity
                                    Quadtree.updateElement presenceOld boundsOld.Box2 presenceNew boundsNew.Box2 element quadree
                                    quadree)
                                (World.getQuadtree world)
                        World.setQuadtree quadree world
                    else
                        let octree =
                            MutantCache.mutateMutant
                                (fun () -> worldOld.WorldExtension.Dispatchers.RebuildOctree worldOld)
                                (fun octree ->
                                    let element = Octelement.make visibleNew staticNew lightProbeNew lightNew presenceNew boundsNew entity
                                    Octree.updateElement presenceOld boundsOld presenceNew boundsNew element octree
                                    octree)
                                (World.getOctree world)
                        World.setOctree octree world

                // fin
                else world

            // fin
            else world

        static member internal viewEntityProperties entity world =
            let state = World.getEntityState entity world
            World.viewSimulantStateProperties state

        /// Clear the content of the clipboard.
        static member clearClipboard (_ : World) =
            Clipboard <- None

        /// Attempt to get the dispatcher name for an entity currently on the world's clipboard.
        static member tryGetEntityDispatcherNameOnClipboard (_ : World) =
            match Clipboard with
            | Some (:? EntityState as entityState) -> Some (getTypeName entityState.Dispatcher)
            | _ -> None

        /// Copy an entity to the world's clipboard.
        static member copyEntityToClipboard entity world =
            let entityState = EntityState.makeFromEntityState None (World.getEntityState entity world)
            Clipboard <- Some (entityState :> obj)

        /// Cut an entity to the world's clipboard.
        static member cutEntityToClipboard (entity : Entity) world =
            World.copyEntityToClipboard entity world
            World.destroyEntityImmediate entity world

        /// Paste an entity from the world's clipboard.
        static member pasteEntityFromClipboard atMouse rightClickPosition snapsEir surnamesOpt (group : Group) world =
            match Clipboard with
            | Some entityStateObj ->
                let entityState = EntityState.makeFromEntityState surnamesOpt (entityStateObj :?> EntityState)
                entityState.Protected <- false // ensure pasted entity is not protected in case user pastes an MMCC entity
                let (position, snapsOpt) =
                    if entityState.Is2d then
                        let viewport = World.getViewport world
                        let eyeCenter = World.getEyeCenter2d world
                        let eyeSize = World.getEyeSize2d world
                        let position =
                            if atMouse
                            then (viewport.MouseToWorld2d (entityState.Absolute, rightClickPosition, eyeCenter, eyeSize)).V3
                            else (viewport.MouseToWorld2d (entityState.Absolute, World.getEyeSize2d world, eyeCenter, eyeSize)).V3
                        match snapsEir with
                        | Left (positionSnap, degreesSnap, scaleSnap) -> (position, Some (positionSnap, degreesSnap, scaleSnap))
                        | Right _ -> (position, None)
                    else
                        let eyeCenter = World.getEyeCenter3d world
                        let eyeRotation = World.getEyeRotation3d world
                        let position =
                            if atMouse then
                                let viewport = Constants.Render.Viewport
                                let ray = viewport.MouseToWorld3d (entityState.Absolute, rightClickPosition, eyeCenter, eyeRotation)
                                let forward = Vector3.Transform (v3Forward, eyeRotation)
                                let plane = plane3 (eyeCenter + forward * Constants.Engine.EyeCenter3dOffset.Z) -forward
                                let intersectionOpt = ray.Intersection plane
                                intersectionOpt.Value
                            else eyeCenter + Vector3.Transform (v3Forward, eyeRotation) * Constants.Engine.EyeCenter3dOffset.Z
                        match snapsEir with
                        | Right (positionSnap, degreesSnap, scaleSnap) -> (position, Some (positionSnap, degreesSnap, scaleSnap))
                        | Left _ -> (position, None)
                entityState.Transform.Position <- position
                match snapsOpt with
                | Some (positionSnap, degreesSnap, scaleSnap) -> entityState.Transform.Snap (positionSnap, degreesSnap, scaleSnap)
                | None -> ()
                entityState.PositionLocal <- v3Zero
                entityState.RotationLocal <- quatIdentity
                entityState.ScaleLocal <- v3One
                entityState.MountOpt <- None
                let entity = Entity (group.GroupAddress <-- rtoa<Entity> entityState.Surnames)
                let world = World.addEntity false entityState entity world
                (Some entity, world)
            | None -> (None, world)

    /// Initialize property getters.
    let private initGetters () =
        EntityGetters.["Dispatcher"] <- fun entity world -> { PropertyType = typeof<EntityDispatcher>; PropertyValue = World.getEntityDispatcher entity world }
        EntityGetters.["Facets"] <- fun entity world -> { PropertyType = typeof<Facet array>; PropertyValue = World.getEntityFacets entity world }
        EntityGetters.["Transform"] <- fun entity world -> { PropertyType = typeof<Transform>; PropertyValue = (World.getEntityState entity world).Transform }
        EntityGetters.["Position"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPosition entity world }
        EntityGetters.["Center"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityCenter entity world }
        EntityGetters.["Bottom"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityBottom entity world }
        EntityGetters.["BottomLeft"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityBottomLeft entity world }
        EntityGetters.["Min"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityMin entity world }
        EntityGetters.["Max"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityMax entity world }
        EntityGetters.["PositionLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityPositionLocal entity world }
        EntityGetters.["CenterLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityCenterLocal entity world }
        EntityGetters.["BottomLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityBottomLocal entity world }
        EntityGetters.["BottomLeftLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityBottomLeftLocal entity world }
        EntityGetters.["MinLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityMinLocal entity world }
        EntityGetters.["MaxLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityMaxLocal entity world }
        EntityGetters.["Rotation"] <- fun entity world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEntityRotation entity world }
        EntityGetters.["RotationLocal"] <- fun entity world -> { PropertyType = typeof<Quaternion>; PropertyValue = World.getEntityRotationLocal entity world }
        EntityGetters.["Scale"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityScale entity world }
        EntityGetters.["ScaleLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityScaleLocal entity world }
        EntityGetters.["Offset"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityOffset entity world }
        EntityGetters.["Angles"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityAngles entity world }
        EntityGetters.["AnglesLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityAnglesLocal entity world }
        EntityGetters.["Degrees"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityDegrees entity world }
        EntityGetters.["DegreesLocal"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntityDegreesLocal entity world }
        EntityGetters.["Size"] <- fun entity world -> { PropertyType = typeof<Vector3>; PropertyValue = World.getEntitySize entity world }
        EntityGetters.["Elevation"] <- fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityElevation entity world }
        EntityGetters.["ElevationLocal"] <- fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityElevationLocal entity world }
        EntityGetters.["Overflow"] <- fun entity world -> { PropertyType = typeof<single>; PropertyValue = World.getEntityOverflow entity world }
        EntityGetters.["PerimeterUnscaled"] <- fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityPerimeterUnscaled entity world }
        EntityGetters.["Perimeter"] <- fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityPerimeter entity world }
        EntityGetters.["PerimeterOriented"] <- fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityPerimeterOriented entity world }
        EntityGetters.["Bounds"] <- fun entity world -> { PropertyType = typeof<Box3>; PropertyValue = World.getEntityBounds entity world }
        EntityGetters.["Presence"] <- fun entity world -> { PropertyType = typeof<Presence>; PropertyValue = World.getEntityPresence entity world }
        EntityGetters.["Absolute"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAbsolute entity world }
        EntityGetters.["Model"] <- fun entity world -> let designerProperty = World.getEntityModelProperty entity world in { PropertyType = designerProperty.DesignerType; PropertyValue = designerProperty.DesignerValue }
        EntityGetters.["MountOpt"] <- fun entity world -> { PropertyType = typeof<Entity Relation option>; PropertyValue = World.getEntityMountOpt entity world }
        EntityGetters.["Imperative"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityImperative entity world }
        EntityGetters.["PublishChangeEvents"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishChangeEvents entity world }
        EntityGetters.["Enabled"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityEnabled entity world }
        EntityGetters.["EnabledLocal"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityEnabledLocal entity world }
        EntityGetters.["Visible"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityVisible entity world }
        EntityGetters.["VisibleLocal"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityVisibleLocal entity world }
        EntityGetters.["Pickable"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPickable entity world }
        EntityGetters.["AlwaysUpdate"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAlwaysUpdate entity world }
        EntityGetters.["AlwaysRender"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityAlwaysRender entity world }
        EntityGetters.["PublishUpdates"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishUpdates entity world }
        EntityGetters.["PublishPostUpdates"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishPostUpdates entity world }
        EntityGetters.["PublishRenders"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPublishRenders entity world }
        EntityGetters.["Protected"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityProtected entity world }
        EntityGetters.["Persistent"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPersistent entity world }
        EntityGetters.["Mounted"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityMounted entity world }
        EntityGetters.["Is2d"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityIs2d entity world }
        EntityGetters.["Is3d"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityIs3d entity world }
        EntityGetters.["Centered"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityCentered entity world }
        EntityGetters.["Static"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityStatic entity world }
        EntityGetters.["LightProbe"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityLightProbe entity world }
        EntityGetters.["Light"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityLight entity world }
        EntityGetters.["Physical"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityPhysical entity world }
        EntityGetters.["Optimized"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityOptimized entity world }
        EntityGetters.["Destroying"] <- fun entity world -> { PropertyType = typeof<bool>; PropertyValue = World.getEntityDestroying entity world }
        EntityGetters.["OverlayNameOpt"] <- fun entity world -> { PropertyType = typeof<string option>; PropertyValue = World.getEntityOverlayNameOpt entity world }
        EntityGetters.["FacetNames"] <- fun entity world -> { PropertyType = typeof<string Set>; PropertyValue = World.getEntityFacetNames entity world }
        EntityGetters.["Order"] <- fun entity world -> { PropertyType = typeof<int64>; PropertyValue = World.getEntityOrder entity world }
        EntityGetters.["Id"] <- fun entity world -> { PropertyType = typeof<Guid>; PropertyValue = World.getEntityId entity world }
        EntityGetters.["Surnames"] <- fun entity world -> { PropertyType = typeof<string array>; PropertyValue = World.getEntitySurnames entity world }
        EntityGetters.["Name"] <- fun entity world -> { PropertyType = typeof<string>; PropertyValue = World.getEntityName entity world }

    /// Initialize property setters.
    let private initSetters () =
        EntitySetters.["Transform"] <- fun property entity world -> let mutable transform = property.PropertyValue :?> Transform in World.setEntityTransformByRef (&transform, World.getEntityState entity world, entity, world)
        EntitySetters.["Position"] <- fun property entity world -> World.setEntityPosition (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Center"] <- fun property entity world -> World.setEntityCenter (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Bottom"] <- fun property entity world -> World.setEntityBottom (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["BottomLeft"] <- fun property entity world -> World.setEntityBottomLeft (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Min"] <- fun property entity world -> World.setEntityMin (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Max"] <- fun property entity world -> World.setEntityMax (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["PositionLocal"] <- fun property entity world -> World.setEntityPositionLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["CenterLocal"] <- fun property entity world -> World.setEntityCenterLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["BottomLocal"] <- fun property entity world -> World.setEntityBottomLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["BottomLeftLocal"] <- fun property entity world -> World.setEntityBottomLeftLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["MinLocal"] <- fun property entity world -> World.setEntityMinLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["MaxLocal"] <- fun property entity world -> World.setEntityMaxLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Scale"] <- fun property entity world -> World.setEntityScale (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["ScaleLocal"] <- fun property entity world -> World.setEntityScaleLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Rotation"] <- fun property entity world -> World.setEntityRotation (property.PropertyValue :?> Quaternion) entity world
        EntitySetters.["RotationLocal"] <- fun property entity world -> World.setEntityRotationLocal (property.PropertyValue :?> Quaternion) entity world
        EntitySetters.["Offset"] <- fun property entity world -> World.setEntityOffset (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Angles"] <- fun property entity world -> World.setEntityAngles (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["AnglesLocal"] <- fun property entity world -> World.setEntityAnglesLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Degrees"] <- fun property entity world -> World.setEntityDegrees (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["DegreesLocal"] <- fun property entity world -> World.setEntityDegreesLocal (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Size"] <- fun property entity world -> World.setEntitySize (property.PropertyValue :?> Vector3) entity world
        EntitySetters.["Elevation"] <- fun property entity world -> World.setEntityElevation (property.PropertyValue :?> single) entity world
        EntitySetters.["ElevationLocal"] <- fun property entity world -> World.setEntityElevationLocal (property.PropertyValue :?> single) entity world
        EntitySetters.["Overflow"] <- fun property entity world -> World.setEntityOverflow (property.PropertyValue :?> single) entity world
        EntitySetters.["PerimeterUnscaled"] <- fun property entity world -> World.setEntityPerimeterUnscaled (property.PropertyValue :?> Box3) entity world
        EntitySetters.["Perimeter"] <- fun property entity world -> World.setEntityPerimeter (property.PropertyValue :?> Box3) entity world
        EntitySetters.["Presence"] <- fun property entity world -> World.setEntityPresence (property.PropertyValue :?> Presence) entity world
        EntitySetters.["Absolute"] <- fun property entity world -> World.setEntityAbsolute (property.PropertyValue :?> bool) entity world
        EntitySetters.["Model"] <- fun property entity world -> World.setEntityModelProperty false { DesignerType = property.PropertyType; DesignerValue = property.PropertyValue } entity world
        EntitySetters.["MountOpt"] <- fun property entity world -> World.setEntityMountOpt (property.PropertyValue :?> Entity Relation option) entity world
        EntitySetters.["Imperative"] <- fun property entity world -> World.setEntityImperative (property.PropertyValue :?> bool) entity world
        EntitySetters.["Enabled"] <- fun property entity world -> World.setEntityEnabled (property.PropertyValue :?> bool) entity world
        EntitySetters.["EnabledLocal"] <- fun property entity world -> World.setEntityEnabledLocal (property.PropertyValue :?> bool) entity world
        EntitySetters.["Visible"] <- fun property entity world -> World.setEntityVisible (property.PropertyValue :?> bool) entity world
        EntitySetters.["VisibleLocal"] <- fun property entity world -> World.setEntityVisibleLocal (property.PropertyValue :?> bool) entity world
        EntitySetters.["Pickable"] <- fun property entity world -> World.setEntityPickable (property.PropertyValue :?> bool) entity world
        EntitySetters.["Centered"] <- fun property entity world -> World.setEntityCentered (property.PropertyValue :?> bool) entity world
        EntitySetters.["Static"] <- fun property entity world -> World.setEntityStatic (property.PropertyValue :?> bool) entity world
        EntitySetters.["LightProbe"] <- fun property entity world -> World.setEntityLightProbe (property.PropertyValue :?> bool) entity world
        EntitySetters.["Light"] <- fun property entity world -> World.setEntityLight (property.PropertyValue :?> bool) entity world
        EntitySetters.["AlwaysUpdate"] <- fun property entity world -> World.setEntityAlwaysUpdate (property.PropertyValue :?> bool) entity world
        EntitySetters.["AlwaysRender"] <- fun property entity world -> World.setEntityAlwaysRender (property.PropertyValue :?> bool) entity world
        EntitySetters.["Persistent"] <- fun property entity world -> World.setEntityPersistent (property.PropertyValue :?> bool) entity world

    /// Initialize getters and setters
    let internal init () =
        initGetters ()
        initSetters ()