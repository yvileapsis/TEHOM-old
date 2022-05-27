﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2020.

namespace Nu
open System
open System.Numerics
open Prime
open Nu

/// Masks for Transform flags.
module TransformMasks =

    // OPTIMIZATION: Transform flag bit-masks for performance.
    let [<Literal>] ActiveMask =                    0b000000000000000000001u
    let [<Literal>] DirtyMask =                     0b000000000000000000010u
    let [<Literal>] InvalidatedMask =               0b000000000000000000100u
    let [<Literal>] OmnipresentMask =               0b000000000000000001000u
    let [<Literal>] AbsoluteMask =                  0b000000000000000010000u
    let [<Literal>] ImperativeMask =                0b000000000000000100000u
    let [<Literal>] PublishChangeBindingsMask =     0b000000000000001000000u
    let [<Literal>] PublishChangeEventsMask =       0b000000000000010000000u
    let [<Literal>] EnabledMask =                   0b000000000000100000000u
    let [<Literal>] VisibleMask =                   0b000000000001000000000u
    let [<Literal>] AlwaysUpdateMask =              0b000000000010000000000u
    let [<Literal>] PublishUpdatesMask =            0b000000000100000000000u
    let [<Literal>] PublishPostUpdatesMask =        0b000000001000000000000u
    let [<Literal>] PersistentMask =                0b000000010000000000000u
    let [<Literal>] IgnorePropertyBindingsMask =    0b000000100000000000000u
    let [<Literal>] MountedMask =                   0b000001000000000000000u
    let [<Literal>] EnabledLocalMask =              0b000010000000000000000u
    let [<Literal>] VisibleLocalMask =              0b000100000000000000000u
    let [<Literal>] RotationMatrixDirtyMask =       0b001000000000000000000u
    let [<Literal>] AffineMatrixDirtyMask =         0b010000000000000000000u
    let [<Literal>] DefaultFlags =                  0b000110010001100100001u

// NOTE: opening this in order to make the Transform property implementations reasonably succinct.
open TransformMasks

/// Carries transformation data specific to an Entity.
type [<NoEquality; NoComparison>] Transform =
    struct
        // cache line 1
        val mutable private Flags_ : uint
        val mutable private Position_ : Vector3
        val mutable private Rotation_ : Quaternion
        // cache line 2
        val mutable private Scale_ : Vector3
        val mutable private Offset_ : Vector3
        val mutable private RotationMatrixOpt_ : Matrix4x4 ref
        val mutable private AffineMatrixOpt_ : Matrix4x4 ref
        // cache line 3
        val mutable private Angles_ : Vector3
        val mutable private Size_ : Vector3
        val mutable private Overflow_ : single
        val mutable private Elevation_ : single
        end

    member this.Active with get () = this.Flags_ &&& ActiveMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ActiveMask else this.Flags_ &&& ~~~ActiveMask
    member this.Dirty with get () = this.Flags_ &&& DirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| DirtyMask else this.Flags_ &&& ~~~DirtyMask
    member this.Invalidated with get () = this.Flags_ &&& InvalidatedMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| InvalidatedMask else this.Flags_ &&& ~~~InvalidatedMask
    member this.Omnipresent with get () = this.Flags_ &&& OmnipresentMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| OmnipresentMask else this.Flags_ &&& ~~~OmnipresentMask
    member this.Absolute with get () = this.Flags_ &&& AbsoluteMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| AbsoluteMask else this.Flags_ &&& ~~~AbsoluteMask
    member this.Imperative with get () = this.Flags_ &&& ImperativeMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ImperativeMask else this.Flags_ &&& ~~~ImperativeMask
    member this.PublishChangeBindings with get () = this.Flags_ &&& PublishChangeBindingsMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishChangeBindingsMask else this.Flags_ &&& ~~~PublishChangeBindingsMask
    member this.PublishChangeEvents with get () = this.Flags_ &&& PublishChangeEventsMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishChangeEventsMask else this.Flags_ &&& ~~~PublishChangeEventsMask
    member this.Enabled with get () = this.Flags_ &&& EnabledMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| EnabledMask else this.Flags_ &&& ~~~EnabledMask
    member this.Visible with get () = this.Flags_ &&& VisibleMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| VisibleMask else this.Flags_ &&& ~~~VisibleMask
    member this.AlwaysUpdate with get () = this.Flags_ &&& AlwaysUpdateMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| AlwaysUpdateMask else this.Flags_ &&& ~~~AlwaysUpdateMask
    member this.PublishUpdates with get () = this.Flags_ &&& PublishUpdatesMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishUpdatesMask else this.Flags_ &&& ~~~PublishUpdatesMask
    member this.PublishPostUpdates with get () = this.Flags_ &&& PublishPostUpdatesMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PublishPostUpdatesMask else this.Flags_ &&& ~~~PublishPostUpdatesMask
    member this.Persistent with get () = this.Flags_ &&& PersistentMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| PersistentMask else this.Flags_ &&& ~~~PersistentMask
    member this.IgnorePropertyBindings with get () = this.Flags_ &&& IgnorePropertyBindingsMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| IgnorePropertyBindingsMask else this.Flags_ &&& ~~~IgnorePropertyBindingsMask
    member this.Mounted with get () = this.Flags_ &&& MountedMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| MountedMask else this.Flags_ &&& ~~~MountedMask
    member this.EnabledLocal with get () = this.Flags_ &&& EnabledLocalMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| EnabledLocalMask else this.Flags_ &&& ~~~EnabledLocalMask
    member this.VisibleLocal with get () = this.Flags_ &&& VisibleLocalMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| VisibleLocalMask else this.Flags_ &&& ~~~VisibleLocalMask
    member this.RotationMatrixDirty with get () = this.Flags_ &&& RotationMatrixDirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| RotationMatrixDirtyMask else this.Flags_ &&& ~~~RotationMatrixDirtyMask
    member this.AffineMatrixDirty with get () = this.Flags_ &&& AffineMatrixDirtyMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| AffineMatrixDirtyMask else this.Flags_ &&& ~~~AffineMatrixDirtyMask
    member this.Optimized with get () = this.Imperative && this.Omnipresent && not this.PublishChangeBindings && not this.PublishChangeEvents // TODO: see if I can remove all conditionals from here.

    member this.Position with get () = this.Position_ and set value = this.Position_ <- value; this.AffineMatrixDirty <- true
    member this.Scale with get () = this.Scale_ and set value = this.Scale_ <- value; this.AffineMatrixDirty <- true
    member this.Offset with get () = this.Offset_ and set value = this.Offset_ <- value; this.AffineMatrixDirty <- true
    member this.Size with get () = this.Size_ and set value = this.Size_ <- value; this.AffineMatrixDirty <- true
    member this.Overflow with get () = this.Overflow_ and set value = this.Overflow_ <- value; this.AffineMatrixDirty <- true
    member this.Elevation with get () = this.Elevation_ and set value = this.Elevation_ <- value

    member this.Rotation
        with get () = this.Rotation_
        and set value =
            this.Rotation_ <- value
            let rollPitchYaw = value.RollPitchYaw
            this.Angles_.X <- rollPitchYaw.X
            this.Angles_.Y <- rollPitchYaw.Y
            this.Angles_.Z <- rollPitchYaw.Z
            this.RotationMatrixDirty <- true
            this.AffineMatrixDirty <- true

    member this.Angles
        with get () = this.Angles_
        and set value =
            this.Angles_ <- value
            this.Rotation_ <- value.RollPitchYaw
            this.RotationMatrixDirty <- true
            this.AffineMatrixDirty <- true

    member this.RotationMatrix =
        if this.RotationMatrixDirty then this.RotationMatrixOpt_ <- ref (Matrix4x4.CreateFromQuaternion this.Rotation_)
        this.RotationMatrixOpt_.Value

    member this.AffineMatrix =
        if this.AffineMatrixDirty then
            // TODO: 3D: optimize this hella!
            let positionMatrix = Matrix4x4.CreateTranslation this.Position_
            let rotationMatrix = this.RotationMatrix
            let scaleMatrix = Matrix4x4.CreateScale this.Scale_
            this.AffineMatrixOpt_ <- ref (positionMatrix * rotationMatrix * scaleMatrix)
        this.AffineMatrixOpt_.Value

    member this.Right = Vector3 (this.RotationMatrix.M11, this.RotationMatrix.M12, this.RotationMatrix.M13) // TODO: implement Row properties.
    member this.Up = Vector3 (this.RotationMatrix.M21, this.RotationMatrix.M22, this.RotationMatrix.M23)
    member this.Forward = -Vector3 (this.RotationMatrix.M31, this.RotationMatrix.M32, this.RotationMatrix.M33)
    member this.Left = -this.Right
    member this.Down = -this.Up
    member this.Backward = -this.Forward

    member this.PerimeterUnscaled
        with get () =
            let size = this.Size_
            let extent = size * 0.5f
            let alpha = this.Position_ - extent
            let offset = this.Offset_ * size
            let position = alpha + offset
            Box3 (position, size)
        and set (value : Box3) =
            let size = value.Size
            let extent = size * 0.5f
            let offset = this.Offset_ * size
            let position = value.Position + extent - offset
            this.Position_ <- position
            this.Size <- size

    member this.Perimeter
        with get () =
            let scale = this.Scale_
            let sizeScaled = this.Size_ * scale
            let extentScaled = sizeScaled * 0.5f
            let alphaScaled = this.Position_ - extentScaled
            let offsetScaled = this.Offset_ * sizeScaled
            let position = alphaScaled + offsetScaled
            Box3 (position, sizeScaled)
        and set (value : Box3) =
            this.Scale_ <- Vector3.One
            this.PerimeterUnscaled <- value

    member this.PerimeterCenter
        with get () =
            let perimeter = this.Perimeter
            perimeter.Center
        and set (value : Vector3) =
            this.Scale_ <- Vector3.One
            let perimeter = this.Perimeter
            let perimeterCenter = perimeter.Translate (value - perimeter.Center)
            this.PerimeterUnscaled <- perimeterCenter

    member this.PerimeterBottom
        with get () =
            let perimeter = this.Perimeter
            perimeter.Bottom
        and set (value : Vector3) =
            this.Scale_ <- Vector3.One
            let perimeter = this.Perimeter
            let perimeterBottom = perimeter.Translate (value - perimeter.Bottom)
            this.PerimeterUnscaled <- perimeterBottom

    member this.PerimeterOriented =
        // TODO: 3D: consider caching this to elide allocation and computation!
        let perimeter = this.Perimeter
        let rotation = this.Rotation_
        if not rotation.IsIdentity then
            let pivot = this.Pivot
            let corners = perimeter.Corners
            let mutable minX = Single.MaxValue
            let mutable minY = Single.MaxValue
            let mutable minZ = Single.MaxValue
            let mutable maxX = Single.MinValue
            let mutable maxY = Single.MinValue
            let mutable maxZ = Single.MinValue
            for i in 0 .. corners.Length - 1 do
                let corner = &corners.[i]
                corner <- Vector3.Transform (corner - pivot, rotation) + pivot
                minX <- min minX corner.X
                minY <- min minY corner.Y
                minZ <- min minZ corner.Z
                maxX <- max maxX corner.X
                maxY <- max maxY corner.Y
                maxZ <- max maxZ corner.Z
            Box3 (minX, minY, minZ, maxX - minX, maxY - minY, maxZ - minZ)
        else perimeter

    member this.Bounds =
        let perimeterOriented = this.PerimeterOriented
        let sizeOverflowed = perimeterOriented.Size * this.Overflow_
        let center = perimeterOriented.Center
        let positionOverflowed = center - sizeOverflowed * 0.5f
        Box3 (positionOverflowed, sizeOverflowed)

    member this.Pivot =
        this.Position_ + this.Offset_ * this.Size_

    // TODO: scale snapping.
    member this.Snap (positionSnap, rotationSnap) =
        this.Position <- Math.snapF3d positionSnap this.Position
        this.Angles <- Math.snapR3d rotationSnap this.Angles

    member this.InvalidateFast () =
        this.Flags_ <- this.Flags_ ||| TransformMasks.InvalidatedMask

    /// Test transforms for equality.
    static member equalsByRef (left : Transform inref, right : Transform inref) =
        left.Flags_ = right.Flags_ &&
        left.Position_.Equals right.Position_ &&
        left.Rotation_.Equals right.Rotation_ &&
        left.Scale_.Equals right.Scale_ &&
        left.Offset_.Equals right.Offset_ &&
        left.Angles_.Equals right.Angles_ &&
        left.Size_.Equals right.Size_ &&
        left.Elevation_ = right.Elevation_ &&
        left.Overflow_ = right.Overflow_

    /// Test transforms for equality.
    static member inline equals (left : Transform) (right : Transform) =
        Transform.equalsByRef (&left, &right)

    /// Assign the value of the left transform to the right.
    static member assignByRef (source : Transform inref, target : Transform byref) =
        target.Flags_ <- source.Flags_
        target.Position_ <- source.Position_
        target.Rotation_ <- source.Rotation_
        target.Scale_ <- source.Scale_
        target.Offset_ <- source.Offset_
        if notNull (source.RotationMatrixOpt_ :> obj) then target.RotationMatrixOpt_ <- ref source.RotationMatrixOpt_.Value
        if notNull (source.AffineMatrixOpt_ :> obj) then target.AffineMatrixOpt_ <- ref source.AffineMatrixOpt_.Value
        target.Angles_ <- source.Angles_
        target.Size_ <- source.Size_
        target.Elevation_ <- source.Elevation_
        target.Overflow_ <- source.Overflow_

    /// Assign the value of the left transform to the right.
    static member inline assign (source : Transform, target : Transform byref) =
        Transform.assignByRef (&source, &target)

    /// Make an empty transform.
    static member inline makeEmpty () =
        Unchecked.defaultof<Transform>

    /// Make a transform with default values.
    static member makeDefault offset =
        let mutable transform = Unchecked.defaultof<Transform>
        transform.Flags_ <- DefaultFlags
        transform.Rotation_ <- Quaternion.Identity
        transform.Scale_ <- Vector3.One
        transform.Offset_ <- offset
        transform.Size_ <- Vector3.One
        transform.Overflow_ <- 1.0f
        transform

    /// Make a transform based on a perimeter.
    static member makePerimeter (perimeter : Box3) offset elevation absolute =
        let mutable transform = Unchecked.defaultof<Transform>
        transform.Flags_ <- DefaultFlags
        transform.Position_ <- perimeter.Position
        transform.Rotation_ <- Quaternion.Identity
        transform.Scale_ <- v3One
        transform.Offset_ <- offset
        transform.Size_ <- perimeter.Size
        transform.Angles_ <- v3Zero
        transform.Elevation_ <- elevation
        transform.Absolute <- absolute
        transform

    /// Make a transform based human-intuited values.
    static member makeIntuitive position scale offset size angles elevation absolute =
        let mutable transform = Transform.makeDefault offset
        transform.Flags_ <- DefaultFlags
        transform.Position_ <- position
        transform.Scale_ <- scale
        transform.Size_ <- size
        transform.Elevation_ <- elevation
        transform.Angles <- angles
        transform.Absolute <- absolute
        transform

    interface Transform Component with
        member this.TypeName = nameof Transform
        member this.Active with get () = this.Flags_ &&& ActiveMask <> 0u and set value = this.Flags_ <- if value then this.Flags_ ||| ActiveMask else this.Flags_ &&& ~~~ActiveMask