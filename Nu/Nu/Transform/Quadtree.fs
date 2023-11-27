﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.Collections
open System.Collections.Generic
open System.Numerics
open Prime

/// Masks for Quadelement flags.
module QuadelementMasks =

    // OPTIMIZATION: Quadelement flag bit-masks for performance.
    let [<Literal>] VisibleMask =   0b00000001u

// NOTE: opening this in order to make the Quadelement property implementations reasonably succinct.
open QuadelementMasks

[<RequireQualifiedAccess>]
module Quadelement =

    /// An element in an quadtree.
    /// NOTE: we intentionally use incomplete equality semantics here so these can be stored in a HashSet.
    type [<CustomEquality; NoComparison; Struct>] Quadelement<'e when 'e : equality> = 
        private
            { HashCode_ : int // OPTIMIZATION: cache hash code to increase look-up speed.
              Flags_ : uint
              Entry_ : 'e }
        member this.Visible = this.Flags_ &&& VisibleMask <> 0u
        member this.Entry = this.Entry_
        override this.GetHashCode () = this.HashCode_
        override this.Equals that = match that with :? Quadelement<'e> as that -> this.Entry_.Equals that.Entry_ | _ -> false
        static member make visible (entry : 'e) =
            let hashCode = entry.GetHashCode ()
            let flags = if visible then VisibleMask else 0u
            { HashCode_ = hashCode; Flags_ = flags; Entry_ = entry }

/// An element in a quadree.
type Quadelement<'e when 'e : equality> = Quadelement.Quadelement<'e>

/// Equality compares two quadelements.
type QuadelementEqualityComparer<'e when 'e : equality> () =
    interface 'e Quadelement IEqualityComparer with
        member this.Equals (left, right) = left.Entry = right.Entry // OPTIMIZATION: inline equality to avoid allocation.
        member this.GetHashCode element = element.GetHashCode ()

[<RequireQualifiedAccess>]
module internal Quadnode =

    type [<Struct>] internal Quadnode<'e when 'e : equality> =
        private
            { Id_ : uint64
              Depth_ : int
              Bounds_ : Box2
              Children_ : ValueEither<'e Quadnode array, 'e Quadelement HashSet> }

        member this.Id = this.Id_

    let internal atPoint (point : Vector2) (node : 'e Quadnode inref) =
        node.Bounds_.Intersects point

    let internal isIntersectingBounds (bounds : Box2) (node : 'e Quadnode inref) =
        node.Bounds_.Intersects bounds

    let inline internal containsBounds (bounds : Box2) (node : 'e Quadnode inref) =
        node.Bounds_.Contains bounds = ContainmentType.Contains

    let rec internal addElement bounds (element : 'e Quadelement inref) (node : 'e Quadnode inref) =
        if isIntersectingBounds bounds &node then
            match node.Children_ with
            | ValueLeft nodes ->
                for i in 0 .. dec nodes.Length do
                    let node = &nodes.[i]
                    addElement bounds &element &node
            | ValueRight elements ->
                elements.Remove element |> ignore
                elements.Add element |> ignore

    let rec internal removeElement bounds (element : 'e Quadelement inref) (node : 'e Quadnode inref) =
        if isIntersectingBounds bounds &node then
            match node.Children_ with
            | ValueLeft nodes ->
                for i in 0 .. dec nodes.Length do
                    let node = &nodes.[i]
                    removeElement bounds &element &node
            | ValueRight elements -> elements.Remove element |> ignore

    let rec internal updateElement boundsOld boundsNew (element : 'e Quadelement inref) (node : 'e Quadnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingBounds boundsOld &node || isIntersectingBounds boundsNew &node then
                    updateElement boundsOld boundsNew &element &node
        | ValueRight elements ->
            if isIntersectingBounds boundsNew &node then
                elements.Remove element |> ignore
                elements.Add element |> ignore
            elif isIntersectingBounds boundsOld &node then
                elements.Remove element |> ignore

    let rec internal getElementsAtPoint point (set : 'e Quadelement HashSet) (node : 'e Quadnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if atPoint point &node then
                    getElementsAtPoint point set &node
        | ValueRight elements ->
            for element in elements do
                set.Add element |> ignore

    let rec internal getElementsInBounds bounds (set : 'e Quadelement HashSet) (node : 'e Quadnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                if isIntersectingBounds bounds &node then
                    getElementsInBounds bounds set &node
        | ValueRight elements ->
            for element in elements do
                set.Add element |> ignore

    let rec internal getElements (set : 'e Quadelement HashSet) (node : 'e Quadnode inref) =
        match node.Children_ with
        | ValueLeft nodes ->
            for i in 0 .. dec nodes.Length do
                let node = &nodes.[i]
                getElements set &node
        | ValueRight children ->
            set.UnionWith children

    let rec internal make<'e when 'e : equality> comparer depth (bounds : Box2) (leaves : Dictionary<Vector2, 'e Quadnode>) =
        if depth < 1 then failwith "Invalid depth for Quadnode. Expected value of at least 1."
        let granularity = 2
        let childDepth = depth - 1
        let childSize = v2 bounds.Size.X bounds.Size.Y / single granularity
        let children =
            if depth > 1 then
                let (nodes : 'e Quadnode array) =
                    [|for i in 0 .. dec (granularity * granularity) do
                        let childPosition = v2 bounds.Min.X bounds.Min.Y + v2 (childSize.X * single (i % granularity)) (childSize.Y * single (i / granularity))
                        let childBounds = box2 childPosition childSize
                        yield make comparer childDepth childBounds leaves|]
                ValueLeft nodes
            else ValueRight (HashSet<'e Quadelement> (comparer : 'e QuadelementEqualityComparer))
        let node =
            { Id_ = Gen.id64
              Depth_ = depth
              Bounds_ = bounds
              Children_ = children }
        if depth = 1 then leaves.Add (bounds.Min, node)
        node

type internal Quadnode<'e when 'e : equality> = Quadnode.Quadnode<'e>

[<RequireQualifiedAccess>]
module Quadtree =

    /// Provides an enumerator interface to the quadtree queries.
    type internal QuadtreeEnumerator<'e when 'e : equality> (uncullable : 'e Quadelement seq, cullable : 'e Quadelement seq) =

        let uncullableArray = SArray.ofSeq uncullable // eagerly convert to segmented array to keep iteration valid
        let cullableArray = SArray.ofSeq cullable // eagerly convert to segmented array to keep iteration valid
        let mutable cullableEnrValid = false
        let mutable uncullableEnrValid = false
        let mutable cullableEnr = Unchecked.defaultof<_>
        let mutable uncullableEnr = Unchecked.defaultof<_>

        interface 'e Quadelement IEnumerator with
            member this.MoveNext () =
                if not cullableEnrValid then
                    cullableEnr <- cullableArray.GetEnumerator ()
                    cullableEnrValid <- true
                    if not (cullableEnr.MoveNext ()) then
                        uncullableEnr <- uncullableArray.GetEnumerator ()
                        uncullableEnrValid <- true
                        uncullableEnr.MoveNext ()
                    else true
                else
                    if not (cullableEnr.MoveNext ()) then
                        if not uncullableEnrValid then
                            uncullableEnr <- uncullableArray.GetEnumerator ()
                            uncullableEnrValid <- true
                            uncullableEnr.MoveNext ()
                        else uncullableEnr.MoveNext ()
                    else true

            member this.Current =
                if uncullableEnrValid then uncullableEnr.Current
                elif cullableEnrValid then cullableEnr.Current
                else failwithumf ()

            member this.Current =
                (this :> 'e Quadelement IEnumerator).Current :> obj

            member this.Reset () =
                cullableEnrValid <- false
                uncullableEnrValid <- false
                cullableEnr <- Unchecked.defaultof<_>
                uncullableEnr <- Unchecked.defaultof<_>

            member this.Dispose () =
                cullableEnr <- Unchecked.defaultof<_>
                uncullableEnr <- Unchecked.defaultof<_>
            
    /// Provides an enumerable interface to the quadtree queries.
    type internal QuadtreeEnumerable<'e when 'e : equality> (enr : 'e QuadtreeEnumerator) =
        interface IEnumerable<'e Quadelement> with
            member this.GetEnumerator () = enr :> 'e Quadelement IEnumerator
            member this.GetEnumerator () = enr :> IEnumerator

    /// A spatial structure that organizes elements on a 2d plane.
    type [<ReferenceEquality>] Quadtree<'e when 'e : equality> =
        private
            { mutable ElementsModified : bool // OPTIMIZATION: short-circuit queries if tree has never had its elements modified.
              Leaves : Dictionary<Vector2, 'e Quadnode>
              LeafSize : Vector2 // TODO: consider keeping the inverse of this to avoid divides.
              Ubiquitous : 'e Quadelement HashSet
              Node : 'e Quadnode
              Depth : int
              Bounds : Box2 }

    let private findNode (bounds : Box2) tree : 'e Quadnode =
        let offset = -tree.Bounds.Min // use offset to bring div ops into positive space
        let divs = (bounds.Min + offset) / tree.LeafSize
        let evens = v2 (divs.X |> int |> single) (divs.Y |> int |> single)
        let leafKey = evens * tree.LeafSize - offset
        match tree.Leaves.TryGetValue leafKey with
        | (true, leaf) when Quadnode.containsBounds bounds &leaf -> leaf
        | (_, _) -> tree.Node

    /// Add an element with the given presence and bounds to the tree.
    let addElement (presence : Presence) bounds element tree =
        tree.ElementsModified <- true
        if presence.ImposterType || presence.OmnipresentType then
            tree.Ubiquitous.Remove element |> ignore
            tree.Ubiquitous.Add element |> ignore
        else
            if not (Quadnode.isIntersectingBounds bounds &tree.Node) then
                Log.info "Element is outside the quadtree's containment area or is being added redundantly."
                tree.Ubiquitous.Remove element |> ignore
                tree.Ubiquitous.Add element |> ignore
            else
                let node = findNode bounds tree
                Quadnode.addElement bounds &element &node

    /// Remove an element with the given presence and bounds from the tree.
    let removeElement (presence : Presence) bounds element tree =
        tree.ElementsModified <- true
        if presence.ImposterType || presence.OmnipresentType then 
            tree.Ubiquitous.Remove element |> ignore
        else
            if not (Quadnode.isIntersectingBounds bounds &tree.Node) then
                Log.info "Element is outside the quadtree's containment area or is not present for removal."
                tree.Ubiquitous.Remove element |> ignore
            else
                let node = findNode bounds tree
                Quadnode.removeElement bounds &element &node

    /// Update an existing element in the tree.
    let updateElement (presenceOld : Presence) boundsOld (presenceNew : Presence) boundsNew element tree =
        tree.ElementsModified <- true
        let wasInNode = not presenceOld.ImposterType && not presenceOld.OmnipresentType && Quadnode.isIntersectingBounds boundsOld &tree.Node
        let isInNode = not presenceNew.ImposterType && not presenceNew.OmnipresentType && Quadnode.isIntersectingBounds boundsNew &tree.Node
        if wasInNode then
            if isInNode then
                let nodeOld = findNode boundsOld tree
                let nodeNew = findNode boundsNew tree
                if nodeOld.Id <> nodeNew.Id then
                    Quadnode.removeElement boundsOld &element &nodeOld
                    Quadnode.addElement boundsNew &element &nodeNew
                else Quadnode.updateElement boundsOld boundsNew &element &nodeOld
            else
                Quadnode.removeElement boundsOld &element &tree.Node |> ignore
                tree.Ubiquitous.Remove element |> ignore
                tree.Ubiquitous.Add element |> ignore
        else
            if isInNode then
                tree.Ubiquitous.Remove element |> ignore
                Quadnode.addElement boundsNew &element &tree.Node
            else
                tree.Ubiquitous.Remove element |> ignore
                tree.Ubiquitous.Add element |> ignore

    /// Get all of the elements in a tree that are in a node intersected by the given point.
    let getElementsAtPoint point set tree =
        if tree.ElementsModified then
            let node = findNode (box2 point v2Zero) tree
            Quadnode.getElementsAtPoint point set &node
            new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Ubiquitous, set)) :> 'e Quadelement IEnumerable
        else Seq.empty

    /// Get all of the elements in a tree that are in a node intersected by the given bounds.
    let getElementsInBounds bounds set tree =
        if tree.ElementsModified then
            Quadnode.getElementsInBounds bounds set &tree.Node
            new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Ubiquitous, set)) :> 'e Quadelement IEnumerable
        else Seq.empty

    /// Get all of the elements in a tree that are in a node intersected by the given bounds.
    let getElementsInView bounds set tree =
        if tree.ElementsModified then
            Quadnode.getElementsInBounds bounds set &tree.Node
            new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Ubiquitous, set)) :> 'e Quadelement IEnumerable
        else Seq.empty

    /// Get all of the elements in a tree that are in a node intersected by the given bounds.
    let getElementsInPlay bounds set tree =
        if tree.ElementsModified then
            Quadnode.getElementsInBounds bounds set &tree.Node
            new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Ubiquitous, set)) :> 'e Quadelement IEnumerable
        else Seq.empty

    /// Get all of the elements in a tree.
    let getElements (set : _ HashSet) tree =
        if tree.ElementsModified then
            Quadnode.getElements set &tree.Node
            new QuadtreeEnumerable<'e> (new QuadtreeEnumerator<'e> (tree.Ubiquitous, set)) :> 'e Quadelement IEnumerable
        else Seq.empty

    /// Get the size of the tree's leaves.
    let getLeafSize tree =
        tree.LeafSize

    /// Get the depth of the tree.
    let getDepth tree =
        tree.Depth

    /// Get the bounds of the tree.
    let getBounds tree =
        tree.Bounds

    /// Create a Quadtree with the given depth and overall size.
    /// Size dimensions must be a power of two.
    let make<'e when 'e : equality> depth (size : Vector2) =
        if  not (Math.IsPowerOfTwo size.X) ||
            not (Math.IsPowerOfTwo size.Y) then
            failwith "Invalid size for Quadtree. Expected value whose components are a power of two."
        let leaves = dictPlus HashIdentity.Structural []
        let mutable leafSize = size
        for _ in 1 .. dec depth do leafSize <- leafSize * 0.5f
        let comparer = QuadelementEqualityComparer<'e> ()
        let min = size * -0.5f + leafSize * 0.5f // OPTIMIZATION: offset min by half leaf size to minimize margin hits at origin.
        let bounds = box2 min size
        { ElementsModified = false
          Leaves = leaves
          LeafSize = leafSize
          Ubiquitous = HashSet comparer
          Node = Quadnode.make<'e> comparer depth bounds leaves
          Depth = depth
          Bounds = bounds }

/// A spatial structure that organizes elements on a 2d plane.
type Quadtree<'e when 'e : equality> = Quadtree.Quadtree<'e>