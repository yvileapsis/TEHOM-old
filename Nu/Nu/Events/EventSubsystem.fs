﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections.Generic
open Prime

/// Specifies whether a process is live or dead.
type [<StructuralEquality; NoComparison; Struct>] Liveness =
   | Live
   | Dead

/// Describes whether an in-flight event has been resolved or should cascade to down-stream handlers.
type [<StructuralEquality; NoComparison; Struct>] Handling =
    | Resolve
    | Cascade

/// An entry in the subscription map.
type [<ReferenceEquality>] SubscriptionEntry =
    { SubscriptionId : Guid
      Subscriber : Simulant
      CallbackBoxed : obj }

/// The generalized event type (can be used to handle any event).
type Event = Event<obj, Simulant>

/// An event used by the event system.
and Event<'a, 's when 's :> Simulant> =
    { Data : 'a
      Subscriber : 's
      Publisher : Simulant
      Address : 'a Address
      Trace : EventTrace }

[<RequireQualifiedAccess>]
module Event =

    /// Specialize an event.
    let specialize<'a, 's when 's :> Simulant> (evt : Event) : Event<'a, 's> =
        { Data = evt.Data :?> 'a
          Subscriber = evt.Subscriber :?> 's
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }

    /// Generalize an event.
    let generalize (evt : Event<'a, 's>) : Event =
        { Data = evt.Data :> obj
          Subscriber = evt.Subscriber
          Publisher = evt.Publisher
          Address = atoa evt.Address
          Trace = evt.Trace }

/// Filters simulants for publishing.
type 'w PublishFilter =
    Simulant -> 'w -> bool

/// Abstracts over a subscription sorting procedure.
type SubscriptionSorter =
    (Guid * SubscriptionEntry) seq -> obj -> (Guid * SubscriptionEntry) seq

/// Describes an event subscription that can be boxed / unboxed.
type 'w BoxableSubscription =
    Event<obj, Simulant> -> 'w -> Handling * 'w

/// A map of event subscriptions.
type SubscriptionEntries =
    UMap<obj Address, OMap<Guid, SubscriptionEntry>>

/// A map of subscription keys to unsubscription data.
type UnsubscriptionEntries =
    UMap<Guid, obj Address * Simulant>

[<RequireQualifiedAccess>]
module EventSubsystem =

    /// OPTIMIZATION: caches event address for fast wildcard address generation.
    let mutable private EventAddressCaching = false
    let private EventAddressCache = Dictionary<obj, obj> HashIdentity.Structural
    let private EventAddressListCache = Dictionary<obj Address, obj List> HashIdentity.Structural

    /// The event subsystem.
    type [<ReferenceEquality>] EventSubsystem =
        private
            { // cache line 1 (assuming 16 byte header)
              Subscriptions : SubscriptionEntries
              Unsubscriptions : UnsubscriptionEntries
              EventStates : SUMap<Guid, obj>
              EventTracerOpt : (string -> unit) option
              EventFilter : EventFilter.Filter
              GlobalSimulantGeneralized : GlobalSimulantGeneralized }

    /// Get the generalized global simulant of the event system.
    let getGlobalSimulantGeneralized eventSubsystem =
        eventSubsystem.GlobalSimulantGeneralized

    /// Get event state.
    let getEventState<'a> key (eventSubsystem : EventSubsystem) =
        let state = SUMap.find key eventSubsystem.EventStates
        state :?> 'a

    /// Add event state.
    let addEventState<'a> key (state : 'a) (eventSubsystem : EventSubsystem) =
        { eventSubsystem with EventStates = SUMap.add key (state :> obj) eventSubsystem.EventStates }

    /// Remove event state.
    let removeEventState key (eventSubsystem : EventSubsystem) =
        { eventSubsystem with EventStates = SUMap.remove key eventSubsystem.EventStates }

    /// Get subscriptions.
    let getSubscriptions (eventSubsystem : EventSubsystem) =
        eventSubsystem.Subscriptions

    /// Get unsubscriptions.
    let getUnsubscriptions (eventSubsystem : EventSubsystem) =
        eventSubsystem.Unsubscriptions

    /// Set subscriptions.
    let internal setSubscriptions subscriptions (eventSubsystem : EventSubsystem) =
        { eventSubsystem with Subscriptions = subscriptions }

    /// Set unsubscriptions.
    let internal setUnsubscriptions unsubscriptions (eventSubsystem : EventSubsystem) =
        { eventSubsystem with Unsubscriptions = unsubscriptions }

    /// Get how events are being traced.
    let getEventTracerOpt (eventSubsystem : EventSubsystem) =
        eventSubsystem.EventTracerOpt

    /// Set how events are being traced.
    let setEventTracerOpt tracing (eventSubsystem : EventSubsystem) =
        { eventSubsystem with EventTracerOpt = tracing }

    /// Get the state of the event filter.
    let getEventFilter (eventSubsystem : EventSubsystem) =
        eventSubsystem.EventFilter

    /// Set the state of the event filter.
    let setEventFilter filter (eventSubsystem : EventSubsystem) =
        { eventSubsystem with EventFilter = filter }

    /// Set whether event addresses are cached internally.
    let setEventAddressCaching caching =
        if not caching then
            EventAddressCache.Clear ()
            EventAddressListCache.Clear ()
        EventAddressCaching <- caching

    /// Remove from the event address cache all addresses belonging to the given target.
    let cleanEventAddressCache (eventTarget : 'a Address) =
        if EventAddressCaching then
            let eventTargetOa = atooa eventTarget
            match EventAddressListCache.TryGetValue eventTargetOa with
            | (true, entries) ->
                for entry in entries do EventAddressCache.Remove entry |> ignore
                EventAddressListCache.Remove eventTargetOa |> ignore
            | (false, _) -> ()
        else ()

    // NOTE: event addresses are ordered from general to specific. This is so a generalized subscriber can preempt
    // any specific subscribers.
    // OPTIMIZATION: imperative for speed.
    let getEventAddresses1 (eventAddress : 'a Address) =

        // create target event address array
        let eventAddressNames = Address.getNames eventAddress
        let eventAddressNamesLength = eventAddressNames.Length
        let eventAddresses = Array.zeroCreate (inc eventAddressNamesLength)

        // make non-wildcard address the last element
        eventAddresses.[eventAddressNamesLength] <- eventAddress

        // populate wildcard addresses from specific to general
        Array.iteri (fun i _ ->
            let eventAddressNamesAny = Array.zeroCreate eventAddressNamesLength
            Array.Copy (eventAddressNames, 0, eventAddressNamesAny, 0, eventAddressNamesLength)
            eventAddressNamesAny.[i] <- Constants.Engine.WildcardName
            let eventAddressAny = Address.rtoa eventAddressNamesAny
            eventAddresses.[i] <- eventAddressAny)
            eventAddressNames

        // fin
        eventAddresses

    /// Get the wild-carded addresses of an event address.
    let getEventAddresses2 (eventAddress : 'a Address) (_ : EventSubsystem) =
        if EventAddressCaching then
            match EventAddressCache.TryGetValue eventAddress with
            | (false, _) ->
                let eventAddressNames = Address.getNames eventAddress
                let eventAddresses = getEventAddresses1 eventAddress
                match Array.tryFindIndex (fun name -> name = "Event") eventAddressNames with
                | Some eventIndex ->
                    let eventTargetIndex = inc eventIndex
                    if eventTargetIndex < Array.length eventAddressNames then
                        let eventTarget = eventAddressNames |> Array.skip eventTargetIndex |> Address.makeFromArray
                        match EventAddressListCache.TryGetValue eventTarget with
                        | (false, _) -> EventAddressListCache.Add (eventTarget, List [eventAddress :> obj]) |> ignore
                        | (true, list) -> list.Add eventAddress
                        EventAddressCache.Add (eventAddress, eventAddresses)
                    eventAddresses
                | None ->
                    failwith
                        ("The event address '" + scstring eventAddress +
                         "' is missing the 'Event' name. All event addresses must separate the event names from the publisher names with 'Event', " +
                         "like 'Click/Event/Button', or 'Mouse/Left/Down/Event' if there is no publisher.")
            | (true, eventAddressesObj) -> eventAddressesObj :?> 'a Address array
        else getEventAddresses1 eventAddress

    /// Get subscriptions for eventAddress sorted by publishSorter.
    let getSubscriptionsSorted (publishSorter : SubscriptionSorter) eventAddress (eventSubsystem : EventSubsystem) (world : 'w) =
        let eventSubscriptions = getSubscriptions eventSubsystem
        let eventAddresses = getEventAddresses2 eventAddress eventSubsystem
        let subscriptionOpts = Array.map (fun eventAddress -> UMap.tryFind eventAddress eventSubscriptions) eventAddresses
        let subscriptions = subscriptionOpts |> Array.definitize |> Array.map OMap.toSeq |> Seq.concat
        publishSorter subscriptions world

    /// Log an event.
    let logEvent (address : obj Address) (trace : EventTrace) (eventSubsystem : EventSubsystem) =
        match eventSubsystem.EventTracerOpt with
        | Some tracer ->
            let addressStr = scstring address
            let traceRev = List.rev trace // for efficiency during normal execution, trace is cons'd up into a reversed list
            if EventFilter.filter addressStr traceRev eventSubsystem.EventFilter then tracer (addressStr + "|" + scstring traceRev)
        | None -> ()

    /// Make an event delegate.
    let make eventTracerOpt eventFilter globalSimulantGeneralized config =
        let eventSubsystem =
            { Subscriptions = UMap.makeEmpty HashIdentity.Structural config
              Unsubscriptions = UMap.makeEmpty HashIdentity.Structural config
              EventStates = SUMap.makeEmpty HashIdentity.Structural config
              EventTracerOpt = eventTracerOpt
              EventFilter = eventFilter
              GlobalSimulantGeneralized = globalSimulantGeneralized }
        eventSubsystem

    let getSortableSubscriptions
        (getSortPriority : Simulant -> 'w -> IComparable)
        (subscriptionEntries : (Guid * SubscriptionEntry) seq)
        (world : 'w) =
        Seq.map
            (fun (_, subscription : SubscriptionEntry) ->
                // NOTE: we just take the sort priority of the first callback found when callbacks are compressed. This
                // is semantically sub-optimal, but should be fine for all of our cases.
                let priority = getSortPriority subscription.Subscriber world
                struct (priority, subscription))
            subscriptionEntries

    /// Publish an event directly.
    let publishEvent<'a, 'p, 's, 'w when 'p :> Simulant and 's :> Simulant>
        (subscriber : Simulant) (publisher : 'p) (eventData : obj) (eventAddress : 'a Address) eventTrace (subscription : obj) (world : 'w) =
        let evt =
            { Data = eventData
              Subscriber = subscriber
              Publisher = publisher :> Simulant
              Address = atooa eventAddress
              Trace = eventTrace }
        let callableSubscription = subscription :?> 'w BoxableSubscription
        callableSubscription evt world

    /// Sort subscriptions using categorization via the 'by' procedure.
    let sortSubscriptionsBy by (subscriptions : (Guid * SubscriptionEntry) seq) (world : 'w) : seq<Guid * SubscriptionEntry> =
        getSortableSubscriptions by subscriptions world |>
        Array.ofSeq |>
        Array.sortWith (fun (struct ((p : IComparable), _)) (struct ((p2 : IComparable), _)) -> p.CompareTo p2) |>
        Array.map (fun (struct (_, subscription)) -> (subscription.SubscriptionId, subscription)) |>
        Array.toSeq

    /// A 'no-op' for subscription sorting - that is, performs no sorting at all.
    let sortSubscriptionsNone (subscriptions : SubscriptionEntry array) (_ : 'w) =
        subscriptions

/// The event subsystem.
type EventSubsystem = EventSubsystem.EventSubsystem