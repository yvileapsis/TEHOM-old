﻿namespace Nu.Ecs
open System
open System.Collections.Generic
open System.IO
open System.Threading.Tasks
open Prime
open Nu

/// An Ecs event callback.
type private EcsCallback<'d, 'w when 'w : not struct> =
    EcsEvent<'d, 'w> -> Ecs -> 'w -> 'w

/// A scheduled Ecs event callback.
and [<NoEquality; NoComparison>] private EcsCallbackScheduled<'d, 'w when 'w : not struct> =
    { EcsDependencies : Query list
      EcsEvent : EcsEvent<'d, 'w> -> Ecs -> 'w -> obj }

/// The type of Ecs event.
and [<StructuralEquality; NoComparison; Struct>] EcsEventType =
    | GlobalEvent
    | EntityEvent of Entity : EcsEntity
    | ComponentEvent of Entity2 : EcsEntity * ComponentEvent : string

/// An Ecs event.
and [<StructuralEquality; NoComparison; Struct>] EcsEvent =
    { EcsEventName : string
      EcsEventType : EcsEventType }

/// An Ecs event.
and [<NoEquality; NoComparison>] EcsEvent<'d, 'w when 'w : not struct> =
    { EcsEventData : 'd }

/// Data for an Ecs registration event.
and [<NoEquality; NoComparison; Struct>] EcsChangeData =
    { EcsEntity : EcsEntity
      ComponentName : string }

/// Data for an Ecs registration event.
and [<NoEquality; NoComparison; Struct>] EcsRegistrationData =
    { EcsEntity : EcsEntity
      ComponentName : string }

/// The out-of-box events for the Ecs construct.
and [<AbstractClass; Sealed>] EcsEvents =
    static member Update = { EcsEventName = "Update"; EcsEventType = GlobalEvent }
    static member PostUpdate = { EcsEventName = "PostUpdate"; EcsEventType = GlobalEvent }
    static member Actualize = { EcsEventName = "Actualize"; EcsEventType = GlobalEvent }
    static member Register entity compName = { EcsEventName = "Register"; EcsEventType = ComponentEvent (entity, compName) }
    static member Unregistering entity compName = { EcsEventName = "Unregistering"; EcsEventType = ComponentEvent (entity, compName) }
    static member Change entity = { EcsEventName = "Change"; EcsEventType = EntityEvent entity }

/// Identifies an archetype.
and ArchetypeId (terms : Map<string, Term>) =

    let terms = Map.add (nameof EntityId) (Intra (nameof EntityId, typeof<EntityId>)) terms

    let hashCode = hash terms // OPTIMIZATION: hash is cached for speed

    new (intraComponents, subterms : Map<string, Term>) =
        ArchetypeId
            (let intraterms = intraComponents |> Seq.map (fun (compName, compTy) -> (Constants.Ecs.IntraComponentPrefix + compName, Intra (compName, compTy))) |> Map.ofSeq
             intraterms @@ subterms)

    new (intraComponentTypes : Type seq, subterms : Map<string, Term>) =
        ArchetypeId
            (let intraterms = intraComponentTypes |> Seq.map (fun compTy -> let compName = compTy.Name in (Constants.Ecs.IntraComponentPrefix + compName, Intra (compName, compTy))) |> Map.ofSeq
             intraterms @@ subterms)

    member this.Terms : Map<string, Term> =
        terms

    member this.HashCode =
        hashCode

    member this.AddTerm termName term =
        if strEq termName (nameof EntityId) then
            failwith "Cannot update an archetype's EntityId term."
        ArchetypeId (Map.add termName term terms)

    member this.RemoveTerm termName =
        if strEq termName (nameof EntityId) then
            failwith "All archetypes require an EntityId component."
        ArchetypeId (Map.remove termName terms)

    static member equals (left : ArchetypeId) (right : ArchetypeId) =
        left.HashCode = right.HashCode &&
        Term.equalsMany left.Terms right.Terms

    override this.GetHashCode () =
        hashCode

    override this.Equals that =
        match that with
        | :? ArchetypeId as that -> ArchetypeId.equals this that
        | _ -> failwithumf ()

    static member make (intraComponents : obj seq, subterms) =
        let intraComponentTypes = Seq.map getType intraComponents
        let intraComponentNames = Seq.map (fun (ty : Type) -> ty.Name) intraComponentTypes
        let intraComponents = Seq.zip intraComponentNames intraComponentTypes
        ArchetypeId (intraComponents, subterms)

    static member make (intraComponents : (string * obj) seq, subterms) =
        let intraComponents = Seq.map (fun (compName, compValue) -> (compName, getType compValue)) intraComponents
        ArchetypeId (intraComponents, subterms)

    static member make ?subterms =
        ArchetypeId (([] : (string * Type) list), Option.getOrDefault Map.empty subterms)

    static member make<'c when
        'c : struct and 'c :> 'c Component>
        (?compName, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)],
             Option.getOrDefault Map.empty subterms)

    static member make<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (?compName, ?comp2Name, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)
              (Option.getOrDefault typeof<'c2>.Name comp2Name, typeof<'c2>)],
             Option.getOrDefault Map.empty subterms)

    static member make<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (?compName, ?comp2Name, ?comp3Name, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)
              (Option.getOrDefault typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.getOrDefault typeof<'c3>.Name comp3Name, typeof<'c3>)],
             Option.getOrDefault Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)
              (Option.getOrDefault typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.getOrDefault typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.getOrDefault typeof<'c4>.Name comp4Name, typeof<'c4>)],
             Option.getOrDefault Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)
              (Option.getOrDefault typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.getOrDefault typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.getOrDefault typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.getOrDefault typeof<'c5>.Name comp5Name, typeof<'c5>)],
             Option.getOrDefault Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)
              (Option.getOrDefault typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.getOrDefault typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.getOrDefault typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.getOrDefault typeof<'c5>.Name comp5Name, typeof<'c5>)
              (Option.getOrDefault typeof<'c6>.Name comp6Name, typeof<'c6>)],
             Option.getOrDefault Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)
              (Option.getOrDefault typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.getOrDefault typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.getOrDefault typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.getOrDefault typeof<'c5>.Name comp5Name, typeof<'c5>)
              (Option.getOrDefault typeof<'c6>.Name comp6Name, typeof<'c6>)
              (Option.getOrDefault typeof<'c7>.Name comp7Name, typeof<'c7>)],
             Option.getOrDefault Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)
              (Option.getOrDefault typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.getOrDefault typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.getOrDefault typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.getOrDefault typeof<'c5>.Name comp5Name, typeof<'c5>)
              (Option.getOrDefault typeof<'c6>.Name comp6Name, typeof<'c6>)
              (Option.getOrDefault typeof<'c7>.Name comp7Name, typeof<'c7>)
              (Option.getOrDefault typeof<'c8>.Name comp8Name, typeof<'c8>)],
             Option.getOrDefault Map.empty subterms)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?subterms) =
        ArchetypeId
            ([(Option.getOrDefault typeof<'c>.Name compName, typeof<'c>)
              (Option.getOrDefault typeof<'c2>.Name comp2Name, typeof<'c2>)
              (Option.getOrDefault typeof<'c3>.Name comp3Name, typeof<'c3>)
              (Option.getOrDefault typeof<'c4>.Name comp4Name, typeof<'c4>)
              (Option.getOrDefault typeof<'c5>.Name comp5Name, typeof<'c5>)
              (Option.getOrDefault typeof<'c6>.Name comp6Name, typeof<'c6>)
              (Option.getOrDefault typeof<'c7>.Name comp7Name, typeof<'c7>)
              (Option.getOrDefault typeof<'c8>.Name comp8Name, typeof<'c8>)
              (Option.getOrDefault typeof<'c9>.Name comp9Name, typeof<'c9>)],
             Option.getOrDefault Map.empty subterms)

/// A collection of component stores.
and Archetype (archetypeId : ArchetypeId) =

    let mutable freeIndex = 0
    let freeList = hashSetPlus<int> HashIdentity.Structural []
    let entityIdStore = Store<EntityId> "EntityId"
    let stores = Dictionary.singleton StringComparer.Ordinal "EntityId" (entityIdStore :> Store)

    do
        let storeTypeGeneric = typedefof<EntityId Store>
        for termEntry in archetypeId.Terms do
            if termEntry.Key <> nameof EntityId then
                match termEntry.Value with
                | Intra (name, ty)
                | Extra (name, ty, _) ->
                    let storeType = storeTypeGeneric.MakeGenericType [|ty|]
                    let store = Activator.CreateInstance (storeType, name) :?> Store
                    stores.[name] <- store
                | _ -> ()

    member this.Id = archetypeId
    member this.Length = freeIndex
    member this.EntityIdStore = entityIdStore
    member this.Stores = stores
    member this.ComponentNames = hashSetPlus StringComparer.Ordinal stores.Keys

    member private this.Grow () =
        for storeEntry in stores do
            storeEntry.Value.Grow ()

    member private this.AllocIndex entityId =
        let index =
            if freeList.Count > 0 then
                let index = Seq.head freeList
                freeList.Remove index |> ignore<bool>
                index
            else
                match Seq.tryHead stores with
                | Some headStoreEntry ->
                    let index = freeIndex
                    if index = headStoreEntry.Value.Length then this.Grow ()
                    freeIndex <- inc freeIndex
                    index
                | None ->
                    let index = freeIndex
                    freeIndex <- inc freeIndex
                    index
        entityIdStore.[index] <- { Active = true; EntityId = entityId }
        index

    member private this.FreeIndex index =
        entityIdStore.[index] <- { Active = false; EntityId = 0UL }
        if index = dec freeIndex
        then freeIndex <- dec freeIndex
        else freeList.Add index |> ignore<bool>

    member this.Register (comps : Dictionary<string, obj>) entityId =
        let index = this.AllocIndex entityId
        for compEntry in comps do
            stores.[compEntry.Key].SetItem index compEntry.Value
        index

    member this.Unregister (index : int) =
        for storeEntry in stores do
            storeEntry.Value.ZeroItem index
        this.FreeIndex index

    member this.GetComponents index =
        let comps = dictPlus<string, obj> StringComparer.Ordinal []
        for storeEntry in stores do
            comps.Add (storeEntry.Key, storeEntry.Value.[index])
        comps

    member this.Read count (stream : FileStream) =
        let firstIndex = freeIndex
        let lastIndex = freeIndex + count
        match Seq.tryHead stores with
        | Some headStoreEntry ->
            while headStoreEntry.Value.Length <= lastIndex do
                this.Grow ()
        | None -> ()
        for storeEntry in stores do
            let store = storeEntry.Value
            store.Read count freeIndex stream
        freeIndex <- inc lastIndex
        (firstIndex, lastIndex)

/// An archetype-based Ecs construct.
and Ecs () =

    let mutable subscriptionIdCurrent = 0u
    let mutable entityIdCurrent = 0UL
    let archetypes = dictPlus<ArchetypeId, Archetype> HashIdentity.Structural []
    let entitySlots = dictPlus<uint64, EcsEntitySlot> HashIdentity.Structural []
    let componentTypes = dictPlus<string, Type> StringComparer.Ordinal []
    let subscriptions = dictPlus<EcsEvent, Dictionary<uint32, obj>> HashIdentity.Structural []
    let subscribedEntities = dictPlus<EcsEntity, int> HashIdentity.Structural []
    let queries = List<Query> ()

    let createArchetype (archetypeId : ArchetypeId) =
        let archetype = Archetype archetypeId
        archetypes.Add (archetypeId, archetype)
        for query in queries do
            query.TryRegisterArchetype archetype
        archetype

    member private this.AllocSubscriptionId () =
        subscriptionIdCurrent <- inc subscriptionIdCurrent
        if subscriptionIdCurrent = UInt32.MaxValue then failwith "Unbounded use of Ecs subscription ids not supported."
        subscriptionIdCurrent

    member private this.BoxCallback<'d, 'w when 'w : not struct> (callback : EcsCallback<'d, 'w>) =
        let boxableCallback = fun (evt : EcsEvent<obj, 'w>) store ->
            let evt = { EcsEventData = evt.EcsEventData :?> 'd }
            callback evt store
        boxableCallback :> obj

    member private this.RegisterEntityInternal comps archetypeId entity =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let archetypeIndex = archetype.Register comps entity.EntityId
        entitySlots.Add (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })

    member private this.UnregisterEntityInternal entitySlot entity =
        let archetype = entitySlot.Archetype
        let comps = archetype.GetComponents entitySlot.ArchetypeIndex
        entitySlots.Remove entity.EntityId |> ignore<bool>
        archetype.Unregister entitySlot.ArchetypeIndex
        comps

    member this.IndexEntitySlot (entity : EcsEntity) =
        entitySlots.[entity.EntityId]

    member this.Entity =
        entityIdCurrent <- inc entityIdCurrent
        if entityIdCurrent = UInt64.MaxValue then failwith "Unbounded use of Ecs entity ids not supported."
        { EntityId = entityIdCurrent; Ecs = this }

    member this.Publish<'d, 'w when 'w : not struct> event (eventData : 'd) (world : 'w) : 'w =
        // NOTE: we allow some special munging with world here. The callback may choose to ignore the world and return
        // the default of 'w. This works fine so long as 'w is a reference type where null is not a proper value
        // because we restore the original world when a null result is detected. However, this does not work
        // when 'w is a reference type that has null as a proper value because we have no way to do efficiently
        // detect that case. Option would be an example of a reference type with null as a proper value.
        let oldState = world
        let mutable world = world
        match subscriptions.TryGetValue event with
        | (true, callbacks) ->
            for entry in callbacks do
                match entry.Value with
                | :? EcsCallback<obj, 'w> as objCallback ->
                    let evt = { EcsEventData = eventData :> obj }
                    world <- match objCallback evt this world :> obj with null -> oldState | world -> world :?> 'w
                | :? EcsCallback<obj, obj> as objCallback ->
                    let evt = { EcsEventData = eventData } : EcsEvent<obj, obj>
                    world <- match objCallback evt this world with null -> oldState | world -> world :?> 'w
                | _ -> ()
        | (false, _) -> ()
        world

    member this.PublishParallel<'d, 'w when 'w : not struct> event (eventData : 'd) =
        let event = { event with EcsEventName = event.EcsEventName + Constants.Ecs.ParallelEventSuffix }
        let vsync =
            match subscriptions.TryGetValue event with
            | (true, callbacks) ->
                callbacks |>
                Seq.map (fun entry ->
                    Task.Run (fun () ->
                        match entry.Value with
                        | :? EcsCallback<obj, 'w> as objCallback ->
                            let evt = { EcsEventData = eventData :> obj }
                            objCallback evt this Unchecked.defaultof<'w> |> ignore<'w>
                        | :? EcsCallback<obj, obj> as objCallback ->
                            let evt = { EcsEventData = eventData } : EcsEvent<obj, obj>
                            objCallback evt this Unchecked.defaultof<obj> |> ignore<obj>
                        | _ -> ()) |> Vsync.AwaitTask) |>
                Vsync.Parallel
            | (false, _) -> Vsync.Parallel []
        Vsync.RunSynchronously vsync |> ignore<unit array>

    member this.PublishScheduled<'d, 'w when 'w : not struct> event (eventData : 'd) =
        let event = { event with EcsEventName = event.EcsEventName + Constants.Ecs.ScheduledEventSuffix }
        match subscriptions.TryGetValue event with
        | (true, callbacks) ->
            let dependentCallbacks = SegmentedList.make ()
            for entry in callbacks do
                match entry.Value with
                | :? EcsCallbackScheduled<obj, 'w> as objCallback ->
                    SegmentedList.add (objCallback.EcsDependencies, objCallback.EcsEvent :> obj) dependentCallbacks
                | :? EcsCallbackScheduled<obj, obj> as objCallback ->
                    SegmentedList.add (objCallback.EcsDependencies, objCallback.EcsEvent :> obj) dependentCallbacks
                | _ -> ()
            // TODO: add dependencies to graph and solve for order / simultanaity the execute in parallel.
            ignore eventData
        | (false, _) -> ()

    member this.SubscribePlus<'d, 'w when 'w : not struct> subscriptionId event (callback : EcsEvent<'d, 'w> -> Ecs -> 'w -> 'w) =
        let subscriptionId =
            match subscriptions.TryGetValue event with
            | (true, callbacks) ->
                callbacks.Add (subscriptionId, this.BoxCallback<'d, 'w> callback)
                subscriptionId
            | (false, _) ->
                let callbacks = dictPlus HashIdentity.Structural [(subscriptionId, this.BoxCallback<'d, 'w> callback)]
                subscriptions.Add (event, callbacks)
                subscriptionId
        match event.EcsEventType with
        | ComponentEvent (entity, _) ->
            match subscribedEntities.TryGetValue entity with
            | (true, count) -> subscribedEntities.[entity] <- inc count
            | (false, _) -> subscribedEntities.Add (entity, 1)
        | _ -> ()
        subscriptionId

    member this.Subscribe<'d, 'w when 'w : not struct> event callback =
        this.SubscribePlus<'d, 'w> (this.AllocSubscriptionId ()) event callback |> ignore

    member this.Unsubscribe event subscriptionId =
        let result =
            match subscriptions.TryGetValue event with
            | (true, callbacks) -> callbacks.Remove subscriptionId
            | (false, _) -> false
        if result then
            match event.EcsEventType with
            | ComponentEvent (entity, _) ->
                match subscribedEntities.TryGetValue entity with
                | (true, count) ->
                    if count = 1
                    then subscribedEntities.Remove entity |> ignore<bool>
                    else subscribedEntities.[entity] <- inc count
                | (false, _) -> failwith "Subscribed entities count mismatch."
            | _ -> failwith "Subscribed entities count mismatch."
        result

    member this.SubscribeParallelPlus<'d, 'w when 'w : not struct> subscriptionId event (callback : EcsEvent<'d, 'w> -> Ecs -> 'w -> unit) =
        this.SubscribePlus<'d, 'w>
            subscriptionId
            { event with EcsEventName = event.EcsEventName + Constants.Ecs.ParallelEventSuffix }
            (fun evt ecs world -> callback evt ecs world; world)

    member this.SubscribeParallel<'d, 'w when 'w : not struct> event callback =
        this.Subscribe<'d, 'w>
            { event with EcsEventName = event.EcsEventName + Constants.Ecs.ParallelEventSuffix }
            (fun evt ecs world -> callback evt ecs world; world)

    member this.UnsubscribeParallel event subscriptionId =
        this.Unsubscribe
            { event with EcsEventName = event.EcsEventName + Constants.Ecs.ParallelEventSuffix }
            subscriptionId

    member this.RegisterComponentName<'c when 'c : struct and 'c :> 'c Component> componentName =
        match componentTypes.TryGetValue componentName with
        | (true, _) -> failwith "Component type already registered."
        | (false, _) -> componentTypes.Add (componentName, typeof<'c>)

    member this.RegisterTerm (termName : string) term (entity : EcsEntity) =
        if termName.StartsWith Constants.Ecs.IntraComponentPrefix then failwith "Term names that start with '@' are for internal use only."
        if (match term with Intra _ -> true | _ -> false) then failwith "Intra components are for internal use only."
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let comps = this.UnregisterEntityInternal entitySlot entity
            match term with Extra (compName, _, comp) -> comps.Add (compName, comp.Value) | _ -> ()
            let archetypeId = entitySlot.Archetype.Id.AddTerm termName term
            this.RegisterEntityInternal comps archetypeId entity
        | (false, _) ->
            let archetypeId = ArchetypeId.make (Map.singleton termName term)
            let comps = dictPlus StringComparer.Ordinal []
            match term with Extra (compName, _, comp) -> comps.Add (compName, comp.Value) | _ -> ()
            this.RegisterEntityInternal comps archetypeId entity

    member this.UnregisterTerm (termName : string) (entity : EcsEntity) =
        if termName.StartsWith Constants.Ecs.IntraComponentPrefix then failwith "Term names that start with '@' are for internal use only."
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let comps = this.UnregisterEntityInternal entitySlot entity
            let archetypeId = entitySlot.Archetype.Id.RemoveTerm termName
            if archetypeId.Terms.Count > 0 then this.RegisterEntityInternal comps archetypeId entity
        | (false, _) -> ()

    member this.RegisterComponentPlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct>
        compName (comp : 'c) (entity : EcsEntity) (world : 'w) =
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let comps = this.UnregisterEntityInternal entitySlot entity
            comps.Add (compName, comp)
            let archetypeId = entitySlot.Archetype.Id.AddTerm (Constants.Ecs.IntraComponentPrefix + compName) (Intra (compName, typeof<'c>))
            this.RegisterEntityInternal comps archetypeId entity
            let eventData = { EcsEntity = entity; ComponentName = compName }
            this.Publish<EcsRegistrationData, obj> (EcsEvents.Register entity compName) eventData (world :> obj) :?> 'w
        | (false, _) ->
            let archetypeId = ArchetypeId (Map.singleton (Constants.Ecs.IntraComponentPrefix + compName) (Intra (compName, typeof<'c>)))
            let comps = Dictionary.singleton StringComparer.Ordinal compName (comp :> obj)
            this.RegisterEntityInternal comps archetypeId entity
            let eventData = { EcsEntity = entity; ComponentName = compName }
            this.Publish<EcsRegistrationData, obj> (EcsEvents.Register entity compName) eventData (world :> obj) :?> 'w

    member this.RegisterComponent<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct>
        (comp : 'c) (entity : EcsEntity) (world : 'w) =
        this.RegisterComponentPlus<'c, 'w> (typeof<'c>.Name) comp (entity : EcsEntity) world

    member this.UnregisterComponentPlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct>
        compName (entity : EcsEntity) (world : 'w) =
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let eventData = { EcsEntity = entity; ComponentName = compName }
            let world = this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (world :> obj) :?> 'w
            let comps = this.UnregisterEntityInternal entitySlot entity
            let archetypeId = entitySlot.Archetype.Id.RemoveTerm (Constants.Ecs.IntraComponentPrefix + compName)
            if archetypeId.Terms.Count > 0 then
                comps.Remove compName |> ignore<bool>
                this.RegisterEntityInternal comps archetypeId entity
                world
            else world
        | (false, _) -> world

    member this.UnregisterComponent<'c, 'w when
        'c : struct and 'c :> 'c Component and 'w : not struct> (entity : EcsEntity) (world : 'w) =
        this.UnregisterComponentPlus<'c, 'w> typeof<'c>.Name entity world

    member this.RegisterEntity elideEvents comps archetypeId world =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let entity = this.Entity
        let archetypeIndex = archetype.Register comps entity.EntityId
        entitySlots.Add (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
        let mutable world = world
        if not elideEvents then
            for compName in archetype.Stores.Keys do
                let eventData = { EcsEntity = entity; ComponentName = compName }
                world <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (world :> obj) :?> 'w
        (entity, world)

    member this.UnregisterEntity (entity : EcsEntity) (world : 'w) =
        match entitySlots.TryGetValue entity.EntityId with
        | (true, entitySlot) ->
            let archetype = entitySlot.Archetype
            let mutable world = world
            if subscribedEntities.ContainsKey entity then
                for compName in archetype.Stores.Keys do
                    let eventData = { EcsEntity = entity; ComponentName = compName }
                    world <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (world :> obj) :?> 'w
            archetype.Unregister entitySlot.ArchetypeIndex
            world
        | (false, _) -> world

    member this.RegisterEntitiesPlus elideEvents count comps archetypeId (world : 'w) =

        // get archetype
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId

        // register entities to archetype
        let mutable world = world
        let entitys = SegmentedArray.zeroCreate count
        for i in 0 .. dec count do
            let entity = this.Entity
            let archetypeIndex = archetype.Register comps entity.EntityId
            entitySlots.Add (entity.EntityId, { ArchetypeIndex = archetypeIndex; Archetype = archetype })
            entitys.[i] <- entity
            if not elideEvents then
                for compName in archetype.Stores.Keys do
                    let eventData = { EcsEntity = entity; ComponentName = compName }
                    world <- this.Publish<EcsRegistrationData, obj> (EcsEvents.Unregistering entity compName) eventData (world :> obj) :?> 'w

        // fin
        (entitys, world)

    member this.RegisterEntities elideEvents count comps archetypeId world =
        let comps = dictPlus StringComparer.Ordinal (Seq.map (fun comp -> (getTypeName comp, comp)) comps)
        this.RegisterEntitiesPlus elideEvents count comps archetypeId world

    member this.RegisterQuery (query : Query) =
        for archetypeEntry in archetypes do
            query.TryRegisterArchetype archetypeEntry.Value
        queries.Add query
        query

    member this.ReadComponents count archetypeId stream =
        let archetype =
            match archetypes.TryGetValue archetypeId with
            | (true, archetype) -> archetype
            | (false, _) -> createArchetype archetypeId
        let (firstIndex, lastIndex) = archetype.Read count stream
        let entitys = SegmentedArray.zeroCreate count
        for i in firstIndex .. lastIndex do
            let entity = this.Entity
            entitySlots.Add (entity.EntityId, { ArchetypeIndex = i; Archetype = archetype })
            entitys.[i - firstIndex] <- entity
        entitys

/// An entity's slot in an archetype.
and [<StructuralEquality; NoComparison; Struct>] EcsEntitySlot =
    { ArchetypeIndex : int
      Archetype : Archetype }

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")

    member this.ToEntityId ecs =
        { EntityId = this.Archetype.EntityIdStore.[this.ArchetypeIndex].EntityId; Ecs = ecs }

    member this.ValidatePlus compName =
        let stores = this.Archetype.Stores
        stores.ContainsKey compName

    member this.Validate<'c when 'c : struct and 'c :> 'c Component> () =
        this.ValidatePlus typeof<'c>.Name

    member this.ValidateTerm termName =
        let terms = this.Archetype.Id.Terms
        terms.ContainsKey termName

    member this.IndexPlus<'c when 'c : struct and 'c :> 'c Component> compName =
        let stores = this.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = this.ArchetypeIndex
        &store.[i]

    member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
        this.IndexPlus<'c> typeof<'c>.Name

    member this.IndexTerm termName =
        let terms = this.Archetype.Id.Terms
        terms.[termName]

    member this.MutatePlus<'c when 'c : struct and 'c :> 'c Component> compName (comp : 'c) =
        let stores = this.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = this.ArchetypeIndex
        store.[i] <- comp

    member this.Mutate<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) =
        this.MutatePlus<'c> typeof<'c>.Name comp

    member this.Frame (statement : Statement<'c, 's>, ?compName, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke (&store.[i], Option.getOrDefault Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 's>,
         ?compName, ?comp2Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke
            (&store.[i], &store2.[i],
             Option.getOrDefault Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i],
             Option.getOrDefault Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i],
             Option.getOrDefault Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i],
             Option.getOrDefault Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
        let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i],
             Option.getOrDefault Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
        let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
        let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i],
             Option.getOrDefault Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
        let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
        let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
        let store8 = this.IndexStore<'c8> (Option.getOrDefault typeof<'c8>.Name comp8Name) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i],
             Option.getOrDefault Unchecked.defaultof<'s> state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?state : 's) =
        let archetype = this.Archetype
        let archetypeId = archetype.Id
        let stores = archetype.Stores
        let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
        let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
        let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
        let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
        let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
        let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
        let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
        let store8 = this.IndexStore<'c8> (Option.getOrDefault typeof<'c8>.Name comp8Name) archetypeId stores
        let store9 = this.IndexStore<'c9> (Option.getOrDefault typeof<'c9>.Name comp9Name) archetypeId stores
        let i = this.ArchetypeIndex
        statement.Invoke
            (&store.[i], &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i],
             Option.getOrDefault Unchecked.defaultof<'s> state)

and [<StructuralEquality; NoComparison; Struct>] EcsEntity =
    { EntityId : uint64
      Ecs : Ecs }

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")

    member this.ToEntitySlot () =
        this.Ecs.IndexEntitySlot this

    member this.RegisterPlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> compName (comp : 'c) (world : 'w) =
        this.Ecs.RegisterComponentPlus<'c, 'w> compName comp this world

    member this.Register<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> (comp : 'c) (world : 'w) =
        this.Ecs.RegisterComponent<'c, 'w> comp this world

    member this.UnregisterPlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> compName (world : 'w) =
        this.Ecs.UnregisterComponentPlus<'c, 'w> compName this world

    member this.Unregister<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> (world : 'w) =
        this.Ecs.UnregisterComponent<'c, 'w> this world

    member this.RegisterTerm termName term =
        this.Ecs.RegisterTerm termName term this

    member this.UnregisterTerm termName =
        this.Ecs.UnregisterTerm termName this

    member this.ValidatePlus compName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.ValidatePlus compName

    member this.Validate<'c when 'c : struct and 'c :> 'c Component> () =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Validate<'c> ()

    member this.ValidateTerm termName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.ValidateTerm termName

    member this.IndexPlus<'c when 'c : struct and 'c :> 'c Component> compName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.IndexPlus<'c> compName

    member this.Index<'c when 'c : struct and 'c :> 'c Component> () =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Index<'c> ()

    member this.IndexTerm termName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.IndexTerm termName

    member this.UpdateTerm updater termName =
        let entitySlot = this.Ecs.IndexEntitySlot this
        let terms = entitySlot.Archetype.Id.Terms
        let term = updater terms.[termName]
        this.UnregisterTerm termName
        this.RegisterTerm termName term

    member this.MutatePlus<'c when 'c : struct and 'c :> 'c Component> compName (comp : 'c) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.MutatePlus<'c> compName comp

    member this.Mutate<'c when 'c : struct and 'c :> 'c Component> (comp : 'c) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Mutate<'c> comp

    member this.ChangePlus<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> compName (comp : 'c) (world : 'w) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        let stores = entitySlot.Archetype.Stores
        let store = stores.[compName] :?> 'c Store
        let i = entitySlot.ArchetypeIndex
        store.[i] <- comp
        this.Ecs.Publish (EcsEvents.Change this) { EcsEntity = this; ComponentName = compName } world

    member this.Change<'c, 'w when 'c : struct and 'c :> 'c Component and 'w : not struct> (comp : 'c) (world : 'w) =
        this.ChangePlus<'c, 'w> typeof<'c>.Name comp world

    member this.Frame
        (statement : Statement<'c, 's>,
         ?compName, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 's>,
         ?compName, ?comp2Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null comp2Name,
             Option.getOrDefault null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null comp2Name,
             Option.getOrDefault null comp3Name,
             Option.getOrDefault null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null comp2Name,
             Option.getOrDefault null comp3Name,
             Option.getOrDefault null comp4Name,
             Option.getOrDefault null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null comp2Name,
             Option.getOrDefault null comp3Name,
             Option.getOrDefault null comp4Name,
             Option.getOrDefault null comp5Name,
             Option.getOrDefault null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null comp2Name,
             Option.getOrDefault null comp3Name,
             Option.getOrDefault null comp4Name,
             Option.getOrDefault null comp5Name,
             Option.getOrDefault null comp6Name,
             Option.getOrDefault null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null comp2Name,
             Option.getOrDefault null comp3Name,
             Option.getOrDefault null comp4Name,
             Option.getOrDefault null comp5Name,
             Option.getOrDefault null comp6Name,
             Option.getOrDefault null comp7Name,
             Option.getOrDefault null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null comp2Name,
             Option.getOrDefault null comp3Name,
             Option.getOrDefault null comp4Name,
             Option.getOrDefault null comp5Name,
             Option.getOrDefault null comp6Name,
             Option.getOrDefault null comp7Name,
             Option.getOrDefault null comp8Name,
             Option.getOrDefault null state)

    member this.Frame
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 's>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?state : 's) =
        let entitySlot = this.Ecs.IndexEntitySlot this
        entitySlot.Frame
            (statement,
             Option.getOrDefault null compName,
             Option.getOrDefault null comp2Name,
             Option.getOrDefault null comp3Name,
             Option.getOrDefault null comp4Name,
             Option.getOrDefault null comp5Name,
             Option.getOrDefault null comp6Name,
             Option.getOrDefault null comp7Name,
             Option.getOrDefault null comp8Name,
             Option.getOrDefault null comp9Name,
             Option.getOrDefault null state)

and Query (compNames : string HashSet, subqueries : Subquery seq) =

    let archetypes = dictPlus<ArchetypeId, Archetype> HashIdentity.Structural []
    let subqueries = List subqueries

    do
        for compName in compNames do
            subqueries.Add (Tagged (Var (Constants.Ecs.IntraComponentPrefix + compName)))

    member inline private this.IndexStore<'c when 'c : struct and 'c :> 'c Component> compName archetypeId (stores : Dictionary<string, Store>) =
        match stores.TryGetValue compName with
        | (true, store) -> store :?> 'c Store
        | (false, _) -> failwith ("Invalid entity frame for archetype " + scstring archetypeId + ".")

    member this.Archetypes =
        archetypes :> IReadOnlyDictionary<_ , _>

    member this.Subqueries =
        seq subqueries

    member internal this.TryRegisterArchetype (archetype : Archetype) =
        if  not (archetypes.ContainsKey archetype.Id) &&
            Subquery.evalMany archetype.Id.Terms subqueries then
            archetypes.Add (archetype.Id, archetype)

    member this.IndexEntitySlots () =
        let slots = SegmentedList.make ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            for i in 0 .. dec archetype.Length do
                let entityId = archetype.EntityIdStore.[i]
                if entityId.Active then
                    SegmentedList.add { ArchetypeIndex = i; Archetype = archetype } slots
        slots

    member this.IndexEntities ecs =
        let entities = SegmentedList.make ()
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            for i in 0 .. dec archetype.Length do
                let entityId = archetype.EntityIdStore.[i]
                if entityId.Active then
                    SegmentedList.add { EntityId = entityId.EntityId; Ecs = ecs } entities
        entities

    member this.Iterate (statement : Statement<'c, 'w>, ?compName, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, world)
                    i <- inc i
        world

    member this.Iterate
        (statement : Statement<'c, 'c2, 'w>,
         ?compName, ?comp2Name, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], world)
                    i <- inc i
        world

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], &store3.[i], world)
                    i <- inc i
        world

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], world)
                    i <- inc i
        world

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], world)
                    i <- inc i
        world

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], world)
                    i <- inc i
        world

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], world)
                    i <- inc i
        world

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
            let store8 = this.IndexStore<'c8> (Option.getOrDefault typeof<'c8>.Name comp8Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], world)
                    i <- inc i
        world

    member this.Iterate
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?world : 'w) : 'w =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
            let store8 = this.IndexStore<'c8> (Option.getOrDefault typeof<'c8>.Name comp8Name) archetypeId stores
            let store9 = this.IndexStore<'c9> (Option.getOrDefault typeof<'c9>.Name comp9Name) archetypeId stores
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i], world)
                    i <- inc i
        world

    member this.IterateParallel (statement : Statement<'c, 'w>, ?compName, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        archetypes |> Seq.map (fun archetypeEntry ->
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let fn =
                fun () ->
                    lock store $ fun () ->
                        let mutable i = 0
                        while i < store.Length && i < length do
                            let comp = &store.[i]
                            if comp.Active then
                                world <- statement.Invoke (&comp, world)
                                i <- inc i
            (archetypeId, fn)

    member this.IterateParallel
        (statement : Statement<'c, 'c2, 'w>,
         ?compName, ?comp2Name, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            lock store $ fun () ->
            lock store2 $ fun () ->
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        world <- statement.Invoke (&comp, &store2.[i], world)
                        i <- inc i

    member this.IterateParallel
        (statement : Statement<'c, 'c2, 'c3, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            lock store $ fun () ->
            lock store2 $ fun () ->
            lock store3 $ fun () ->
            let mutable i = 0
            while i < store.Length && i < length do
                let comp = &store.[i]
                if comp.Active then
                    world <- statement.Invoke (&comp, &store2.[i], &store3.[i], world)
                    i <- inc i

    member this.IterateParallel
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            lock store $ fun () ->
            lock store2 $ fun () ->
            lock store3 $ fun () ->
            lock store4 $ fun () ->
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], world)
                        i <- inc i

    member this.IterateParallel
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            lock store $ fun () ->
            lock store2 $ fun () ->
            lock store3 $ fun () ->
            lock store4 $ fun () ->
            lock store5 $ fun () ->
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], world)
                        i <- inc i

    member this.IterateParallel
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
            lock store $ fun () ->
            lock store2 $ fun () ->
            lock store3 $ fun () ->
            lock store4 $ fun () ->
            lock store5 $ fun () ->
            lock store6 $ fun () ->
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], world)
                        i <- inc i

    member this.IterateParallel
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
            lock store $ fun () ->
            lock store2 $ fun () ->
            lock store3 $ fun () ->
            lock store4 $ fun () ->
            lock store5 $ fun () ->
            lock store6 $ fun () ->
            lock store7 $ fun () ->
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], world)
                        i <- inc i

    member this.IterateParallel
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
            let store8 = this.IndexStore<'c8> (Option.getOrDefault typeof<'c8>.Name comp8Name) archetypeId stores
            lock store $ fun () ->
            lock store2 $ fun () ->
            lock store3 $ fun () ->
            lock store4 $ fun () ->
            lock store5 $ fun () ->
            lock store6 $ fun () ->
            lock store7 $ fun () ->
            lock store8 $ fun () ->
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], world)
                        i <- inc i

    member this.IterateParallel
        (statement : Statement<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9, 'w>,
         ?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?world : 'w) =
        let mutable world = Option.getOrDefault Unchecked.defaultof<'w> world
        for archetypeEntry in archetypes do
            let archetype = archetypeEntry.Value
            let archetypeId = archetype.Id
            let length = archetype.Length
            let stores = archetype.Stores
            let store = this.IndexStore<'c> (Option.getOrDefault typeof<'c>.Name compName) archetypeId stores
            let store2 = this.IndexStore<'c2> (Option.getOrDefault typeof<'c2>.Name comp2Name) archetypeId stores
            let store3 = this.IndexStore<'c3> (Option.getOrDefault typeof<'c3>.Name comp3Name) archetypeId stores
            let store4 = this.IndexStore<'c4> (Option.getOrDefault typeof<'c4>.Name comp4Name) archetypeId stores
            let store5 = this.IndexStore<'c5> (Option.getOrDefault typeof<'c5>.Name comp5Name) archetypeId stores
            let store6 = this.IndexStore<'c6> (Option.getOrDefault typeof<'c6>.Name comp6Name) archetypeId stores
            let store7 = this.IndexStore<'c7> (Option.getOrDefault typeof<'c7>.Name comp7Name) archetypeId stores
            let store8 = this.IndexStore<'c8> (Option.getOrDefault typeof<'c8>.Name comp8Name) archetypeId stores
            let store9 = this.IndexStore<'c9> (Option.getOrDefault typeof<'c9>.Name comp9Name) archetypeId stores
            lock store $ fun () ->
            lock store2 $ fun () ->
            lock store3 $ fun () ->
            lock store4 $ fun () ->
            lock store5 $ fun () ->
            lock store6 $ fun () ->
            lock store7 $ fun () ->
            lock store8 $ fun () ->
            lock store9 $ fun () ->
                let mutable i = 0
                while i < store.Length && i < length do
                    let comp = &store.[i]
                    if comp.Active then
                        world <- statement.Invoke (&comp, &store2.[i], &store3.[i], &store4.[i], &store5.[i], &store6.[i], &store7.[i], &store8.[i], &store9.[i], world)
                        i <- inc i

    static member make
        (?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural [],
             Option.getOrDefault [] subqueries)

    static member make<'c when
        'c : struct and 'c :> 'c Component>
        (?compName, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName],
             Option.getOrDefault [] subqueries)

    static member make<'c, 'c2 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component>
        (?compName, ?comp2Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName
                 Option.getOrDefault typeof<'c2>.Name comp2Name],
             Option.getOrDefault [] subqueries)

    static member make<'c, 'c2, 'c3 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component>
        (?compName, ?comp2Name, ?comp3Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName
                 Option.getOrDefault typeof<'c2>.Name comp2Name
                 Option.getOrDefault typeof<'c3>.Name comp3Name],
             Option.getOrDefault [] subqueries)

    static member make<'c, 'c2, 'c3, 'c4 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName
                 Option.getOrDefault typeof<'c2>.Name comp2Name
                 Option.getOrDefault typeof<'c3>.Name comp3Name
                 Option.getOrDefault typeof<'c4>.Name comp4Name],
             Option.getOrDefault [] subqueries)

    static member make<'c, 'c2, 'c3, 'c4, 'c5 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName
                 Option.getOrDefault typeof<'c2>.Name comp2Name
                 Option.getOrDefault typeof<'c3>.Name comp3Name
                 Option.getOrDefault typeof<'c4>.Name comp4Name
                 Option.getOrDefault typeof<'c5>.Name comp5Name],
             Option.getOrDefault [] subqueries)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName
                 Option.getOrDefault typeof<'c2>.Name comp2Name
                 Option.getOrDefault typeof<'c3>.Name comp3Name
                 Option.getOrDefault typeof<'c4>.Name comp4Name
                 Option.getOrDefault typeof<'c5>.Name comp5Name
                 Option.getOrDefault typeof<'c6>.Name comp6Name],
             Option.getOrDefault [] subqueries)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName
                 Option.getOrDefault typeof<'c2>.Name comp2Name
                 Option.getOrDefault typeof<'c3>.Name comp3Name
                 Option.getOrDefault typeof<'c4>.Name comp4Name
                 Option.getOrDefault typeof<'c5>.Name comp5Name
                 Option.getOrDefault typeof<'c6>.Name comp6Name
                 Option.getOrDefault typeof<'c7>.Name comp7Name],
             Option.getOrDefault [] subqueries)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName
                 Option.getOrDefault typeof<'c2>.Name comp2Name
                 Option.getOrDefault typeof<'c3>.Name comp3Name
                 Option.getOrDefault typeof<'c4>.Name comp4Name
                 Option.getOrDefault typeof<'c5>.Name comp5Name
                 Option.getOrDefault typeof<'c6>.Name comp6Name
                 Option.getOrDefault typeof<'c7>.Name comp7Name
                 Option.getOrDefault typeof<'c8>.Name comp8Name],
             Option.getOrDefault [] subqueries)

    static member make<'c, 'c2, 'c3, 'c4, 'c5, 'c6, 'c7, 'c8, 'c9 when
        'c : struct and 'c :> 'c Component and
        'c2 : struct and 'c2 :> 'c2 Component and
        'c3 : struct and 'c3 :> 'c3 Component and
        'c4 : struct and 'c4 :> 'c4 Component and
        'c5 : struct and 'c5 :> 'c5 Component and
        'c6 : struct and 'c6 :> 'c6 Component and
        'c7 : struct and 'c7 :> 'c7 Component and
        'c8 : struct and 'c8 :> 'c8 Component and
        'c9 : struct and 'c9 :> 'c9 Component>
        (?compName, ?comp2Name, ?comp3Name, ?comp4Name, ?comp5Name, ?comp6Name, ?comp7Name, ?comp8Name, ?comp9Name, ?subqueries) =
        Query
            (hashSetPlus HashIdentity.Structural
                [Option.getOrDefault typeof<'c>.Name compName
                 Option.getOrDefault typeof<'c2>.Name comp2Name
                 Option.getOrDefault typeof<'c3>.Name comp3Name
                 Option.getOrDefault typeof<'c4>.Name comp4Name
                 Option.getOrDefault typeof<'c5>.Name comp5Name
                 Option.getOrDefault typeof<'c6>.Name comp6Name
                 Option.getOrDefault typeof<'c7>.Name comp7Name
                 Option.getOrDefault typeof<'c8>.Name comp8Name
                 Option.getOrDefault typeof<'c9>.Name comp9Name],
             Option.getOrDefault [] subqueries)