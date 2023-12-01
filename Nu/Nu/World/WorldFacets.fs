﻿// Nu Game Engine.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu
open System
open System.IO
open System.Numerics
open TiledSharp
open Prime
open Nu

/// Declaratively exposes simulant lenses and events.
[<AutoOpen>]
module Declarative =

    /// The global Game simulant.
    let Game = Game.Handle

    /// Declaratively exposes Screen lenses and events.
    let Screen = Unchecked.defaultof<Screen>

    /// Declaratively exposes Group lenses and events.
    let Group = Unchecked.defaultof<Group>

    /// Declaratively exposes Entity lenses and events.
    let Entity = Unchecked.defaultof<Entity>

[<AutoOpen>]
module StaticSpriteFacetModule =

    type Entity with
        member this.GetInsetOpt world : Box2 option = this.Get (nameof this.InsetOpt) world
        member this.SetInsetOpt (value : Box2 option) world = this.Set (nameof this.InsetOpt) value world
        member this.InsetOpt = lens (nameof this.InsetOpt) this this.GetInsetOpt this.SetInsetOpt
        member this.GetStaticImage world : Image AssetTag = this.Get (nameof this.StaticImage) world
        member this.SetStaticImage (value : Image AssetTag) world = this.Set (nameof this.StaticImage) value world
        member this.StaticImage = lens (nameof this.StaticImage) this this.GetStaticImage this.SetStaticImage
        member this.GetColor world : Color = this.Get (nameof this.Color) world
        member this.SetColor (value : Color) world = this.Set (nameof this.Color) value world
        member this.Color = lens (nameof this.Color) this this.GetColor this.SetColor
        member this.GetBlend world : Blend = this.Get (nameof this.Blend) world
        member this.SetBlend (value : Blend) world = this.Set (nameof this.Blend) value world
        member this.Blend = lens (nameof this.Blend) this this.GetBlend this.SetBlend
        member this.GetEmission world : Color = this.Get (nameof this.Emission) world
        member this.SetEmission (value : Color) world = this.Set (nameof this.Emission) value world
        member this.Emission = lens (nameof this.Emission) this this.GetEmission this.SetEmission
        member this.GetFlip world : Flip = this.Get (nameof this.Flip) world
        member this.SetFlip (value : Flip) world = this.Set (nameof this.Flip) value world
        member this.Flip = lens (nameof this.Flip) this this.GetFlip this.SetFlip

    /// Augments an entity with a static sprite.
    type StaticSpriteFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.StaticImage Assets.Default.Box
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Emission Color.Zero
             define Entity.Flip FlipNone]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let staticImage = entity.GetStaticImage world
            let insetOpt = match entity.GetInsetOpt world with Some inset -> ValueSome inset | None -> ValueNone
            let color = entity.GetColor world
            let blend = entity.GetBlend world
            let emission = entity.GetEmission world
            let flip = entity.GetFlip world
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, AssetTag.generalize staticImage, &transform, &insetOpt, staticImage, &color, blend, &emission, flip, world)

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetStaticImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module AnimatedSpriteFacetModule =

    type Entity with
        member this.GetStartTime world : GameTime = this.Get (nameof this.StartTime) world
        member this.SetStartTime (value : GameTime) world = this.Set (nameof this.StartTime) value world
        member this.StartTime = lens (nameof this.StartTime) this this.GetStartTime this.SetStartTime
        member this.GetCelSize world : Vector2 = this.Get (nameof this.CelSize) world
        member this.SetCelSize (value : Vector2) world = this.Set (nameof this.CelSize) value world
        member this.CelSize = lens (nameof this.CelSize) this this.GetCelSize this.SetCelSize
        member this.GetCelCount world : int = this.Get (nameof this.CelCount) world
        member this.SetCelCount (value : int) world = this.Set (nameof this.CelCount) value world
        member this.CelCount = lens (nameof this.CelCount) this this.GetCelCount this.SetCelCount
        member this.GetCelRun world : int = this.Get (nameof this.CelRun) world
        member this.SetCelRun (value : int) world = this.Set (nameof this.CelRun) value world
        member this.CelRun = lens (nameof this.CelRun) this this.GetCelRun this.SetCelRun
        member this.GetAnimationStride world : int = this.Get (nameof this.AnimationStride) world
        member this.SetAnimationStride (value : int) world = this.Set (nameof this.AnimationStride) value world
        member this.AnimationStride = lens (nameof this.AnimationStride) this this.GetAnimationStride this.SetAnimationStride
        member this.GetAnimationDelay world : GameTime = this.Get (nameof this.AnimationDelay) world
        member this.SetAnimationDelay (value : GameTime) world = this.Set (nameof this.AnimationDelay) value world
        member this.AnimationDelay = lens (nameof this.AnimationDelay) this this.GetAnimationDelay this.SetAnimationDelay
        member this.GetAnimationSheet world : Image AssetTag = this.Get (nameof this.AnimationSheet) world
        member this.SetAnimationSheet (value : Image AssetTag) world = this.Set (nameof this.AnimationSheet) value world
        member this.AnimationSheet = lens (nameof this.AnimationSheet) this this.GetAnimationSheet this.SetAnimationSheet

    /// Augments an entity with an animated sprite.
    type AnimatedSpriteFacet () =
        inherit Facet (false)

        static let getSpriteInsetOpt (entity : Entity) world =
            let startTime = entity.GetStartTime world
            let celCount = entity.GetCelCount world
            let celRun = entity.GetCelRun world
            if celCount <> 0 && celRun <> 0 then
                let localTime = world.GameTime - startTime
                let cel = int (localTime / entity.GetAnimationDelay world) % celCount * entity.GetAnimationStride world
                let celSize = entity.GetCelSize world
                let celI = cel % celRun
                let celJ = cel / celRun
                let celX = single celI * celSize.X
                let celY = single celJ * celSize.Y
                let inset = box2 (v2 celX celY) celSize
                Some inset
            else None

        static member Properties =
            [define Entity.StartTime GameTime.zero
             define Entity.CelSize (Vector2 (12.0f, 12.0f))
             define Entity.CelCount 16
             define Entity.CelRun 4
             define Entity.AnimationDelay (GameTime.ofSeconds (1.0f / 15.0f))
             define Entity.AnimationStride 1
             define Entity.AnimationSheet Assets.Default.Block
             define Entity.Color Color.One
             define Entity.Blend Transparent
             define Entity.Emission Color.Zero
             define Entity.Flip FlipNone]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let animationSheet = entity.GetAnimationSheet world
            let insetOpt = match getSpriteInsetOpt entity world with Some inset -> ValueSome inset | None -> ValueNone
            let color = entity.GetColor world
            let blend = entity.GetBlend world
            let emission = entity.GetEmission world
            let flip = entity.GetFlip world
            World.renderLayeredSpriteFast (transform.Elevation, transform.Horizon, AssetTag.generalize animationSheet, &transform, &insetOpt, animationSheet, &color, blend, &emission, flip, world)

        override this.GetQuickSize (entity, world) =
            (entity.GetCelSize world).V3

[<AutoOpen>]
module BasicStaticSpriteEmitterFacetModule =

    type Entity with

        member this.GetSelfDestruct world : bool = this.Get (nameof this.SelfDestruct) world
        member this.SetSelfDestruct (value : bool) world = this.Set (nameof this.SelfDestruct) value world
        member this.SelfDestruct = lens (nameof this.SelfDestruct) this this.GetSelfDestruct this.SetSelfDestruct
        member this.GetEmitterGravity world : Vector3 = this.Get (nameof this.EmitterGravity) world
        member this.SetEmitterGravity (value : Vector3) world = this.Set (nameof this.EmitterGravity) value world
        member this.EmitterGravity = lens (nameof this.EmitterGravity) this this.GetEmitterGravity this.SetEmitterGravity
        member this.GetEmitterImage world : Image AssetTag = this.Get (nameof this.EmitterImage) world
        member this.SetEmitterImage (value : Image AssetTag) world = this.Set (nameof this.EmitterImage) value world
        member this.EmitterImage = lens (nameof this.EmitterImage) this this.GetEmitterImage this.SetEmitterImage
        member this.GetEmitterBlend world : Blend = this.Get (nameof this.EmitterBlend) world
        member this.SetEmitterBlend (value : Blend) world = this.Set (nameof this.EmitterBlend) value world
        member this.EmitterBlend = lens (nameof this.EmitterBlend) this this.GetEmitterBlend this.SetEmitterBlend
        member this.GetEmitterLifeTimeOpt world : GameTime = this.Get (nameof this.EmitterLifeTimeOpt) world
        member this.SetEmitterLifeTimeOpt (value : GameTime) world = this.Set (nameof this.EmitterLifeTimeOpt) value world
        member this.EmitterLifeTimeOpt = lens (nameof this.EmitterLifeTimeOpt) this this.GetEmitterLifeTimeOpt this.SetEmitterLifeTimeOpt
        member this.GetParticleLifeTimeMaxOpt world : GameTime = this.Get (nameof this.ParticleLifeTimeMaxOpt) world
        member this.SetParticleLifeTimeMaxOpt (value : GameTime) world = this.Set (nameof this.ParticleLifeTimeMaxOpt) value world
        member this.ParticleLifeTimeMaxOpt = lens (nameof this.ParticleLifeTimeMaxOpt) this this.GetParticleLifeTimeMaxOpt this.SetParticleLifeTimeMaxOpt
        member this.GetParticleRate world : single = this.Get (nameof this.ParticleRate) world
        member this.SetParticleRate (value : single) world = this.Set (nameof this.ParticleRate) value world
        member this.ParticleRate = lens (nameof this.ParticleRate) this this.GetParticleRate this.SetParticleRate
        member this.GetParticleMax world : int = this.Get (nameof this.ParticleMax) world
        member this.SetParticleMax (value : int) world = this.Set (nameof this.ParticleMax) value world
        member this.ParticleMax = lens (nameof this.ParticleMax) this this.GetParticleMax this.SetParticleMax
        member this.GetBasicParticleSeed world : Particles.BasicParticle = this.Get (nameof this.BasicParticleSeed) world
        member this.SetBasicParticleSeed (value : Particles.BasicParticle) world = this.Set (nameof this.BasicParticleSeed) value world
        member this.BasicParticleSeed = lens (nameof this.BasicParticleSeed) this this.GetBasicParticleSeed this.SetBasicParticleSeed
        member this.GetEmitterConstraint world : Particles.Constraint = this.Get (nameof this.EmitterConstraint) world
        member this.SetEmitterConstraint (value : Particles.Constraint) world = this.Set (nameof this.EmitterConstraint) value world
        member this.EmitterConstraint = lens (nameof this.EmitterConstraint) this this.GetEmitterConstraint this.SetEmitterConstraint
        member this.GetEmitterStyle world : string = this.Get (nameof this.EmitterStyle) world
        member this.SetEmitterStyle (value : string) world = this.Set (nameof this.EmitterStyle) value world
        member this.EmitterStyle = lens (nameof this.EmitterStyle) this this.GetEmitterStyle this.SetEmitterStyle
        member this.GetParticleSystem world : Particles.ParticleSystem = this.Get (nameof this.ParticleSystem) world
        member this.SetParticleSystem (value : Particles.ParticleSystem) world = this.Set (nameof this.ParticleSystem) value world
        member this.ParticleSystem = lens (nameof this.ParticleSystem) this this.GetParticleSystem this.SetParticleSystem

    /// Augments an entity with a basic static sprite emitter.
    type BasicStaticSpriteEmitterFacet () =
        inherit Facet (false)

        static let tryMakeEmitter (entity : Entity) (world : World) =
            World.tryMakeEmitter
                world.GameTime
                (entity.GetEmitterLifeTimeOpt world)
                (entity.GetParticleLifeTimeMaxOpt world)
                (entity.GetParticleRate world)
                (entity.GetParticleMax world)
                (entity.GetEmitterStyle world)
                world |>
            Option.map cast<Particles.BasicStaticSpriteEmitter>

        static let makeEmitter entity world =
            match tryMakeEmitter entity world with
            | Some emitter ->
                let mutable transform = entity.GetTransform world
                { emitter with
                    Body =
                        { Position = transform.Position
                          Scale = transform.Scale
                          Angles = transform.Angles
                          LinearVelocity = v3Zero
                          AngularVelocity = v3Zero
                          Restitution = Constants.Particles.RestitutionDefault }
                    Elevation = transform.Elevation
                    Absolute = transform.Absolute
                    Blend = entity.GetEmitterBlend world
                    Image = entity.GetEmitterImage world
                    ParticleSeed = entity.GetBasicParticleSeed world
                    Constraint = entity.GetEmitterConstraint world }
            | None ->
                Particles.BasicStaticSpriteEmitter.makeEmpty
                    world.GameTime
                    (entity.GetEmitterLifeTimeOpt world)
                    (entity.GetParticleLifeTimeMaxOpt world)
                    (entity.GetParticleRate world)
                    (entity.GetParticleMax world)

        static let updateParticleSystem updater (entity : Entity) world =
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = updater particleSystem
            let world = entity.SetParticleSystem particleSystem world
            world

        static let updateEmitter updater (entity : Entity) world =
            updateParticleSystem (fun particleSystem ->
                match Map.tryFind typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicStaticSpriteEmitter as emitter) ->
                    let emitter = updater emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem)
                entity world

        static let rec processOutput output entity world =
            match output with
            | Particles.OutputSound (volume, sound) -> World.enqueueAudioMessage (PlaySoundMessage { Volume = volume; Sound = sound }) world
            | Particles.OutputEmitter (name, emitter) -> updateParticleSystem (fun ps -> { ps with Emitters = Map.add name emitter ps.Emitters }) entity world
            | Particles.Outputs outputs -> SArray.fold (fun world output -> processOutput output entity world) world outputs

        static let handleEmitterBlendChange evt world =
            let emitterBlend = evt.Data.Value :?> Blend
            let world = updateEmitter (fun emitter -> if emitter.Blend <> emitterBlend then { emitter with Blend = emitterBlend } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterImageChange evt world =
            let emitterImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.Image emitterImage then { emitter with Image = emitterImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterLifeTimeOptChange evt world =
            let emitterLifeTimeOpt = evt.Data.Value :?> GameTime
            let world = updateEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleLifeTimeMaxOptChange evt world =
            let particleLifeTimeMaxOpt = evt.Data.Value :?> GameTime
            let world = updateEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleRateChange evt world =
            let particleRate = evt.Data.Value :?> single
            let world = updateEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleMaxChange evt world =
            let particleMax = evt.Data.Value :?> int
            let world = updateEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicStaticSpriteEmitter.resize particleMax emitter else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleBasicParticleSeedChange evt world =
            let particleSeed = evt.Data.Value :?> Particles.BasicParticle
            let world = updateEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterConstraintChange evt world =
            let emitterConstraint = evt.Data.Value :?> Particles.Constraint
            let world = updateEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterStyleChange evt world =
            let entity = evt.Subscriber
            let emitter = makeEmitter entity world
            let world = updateEmitter (constant emitter) entity world
            (Cascade, world)

        static let handlePositionChange evt world =
            let entity = evt.Subscriber : Entity
            let particleSystem = entity.GetParticleSystem world
            let particleSystem =
                match Map.tryFind typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicStaticSpriteEmitter as emitter) ->
                    let position = entity.GetPosition world
                    let emitter =
                        if v3Neq emitter.Body.Position position
                        then { emitter with Body = { emitter.Body with Position = position }}
                        else emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

        static let handleRotationChange evt world =
            let entity = evt.Subscriber : Entity
            let particleSystem = entity.GetParticleSystem world
            let particleSystem =
                match Map.tryFind typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicStaticSpriteEmitter as emitter) ->
                    let angles = entity.GetAngles world
                    let emitter =
                        if v3Neq emitter.Body.Angles angles
                        then { emitter with Body = { emitter.Body with Angles = angles }}
                        else emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

        static member Properties =
            [define Entity.SelfDestruct false
             define Entity.EmitterBlend Transparent
             define Entity.EmitterImage Assets.Default.Image
             define Entity.EmitterLifeTimeOpt GameTime.zero
             define Entity.ParticleLifeTimeMaxOpt (GameTime.ofSeconds 1.0f)
             define Entity.ParticleRate (match Constants.GameTime.DesiredFrameRate with StaticFrameRate _ -> 1.0f | DynamicFrameRate _ -> 60.0f)
             define Entity.ParticleMax 60
             define Entity.BasicParticleSeed { Life = Particles.Life.make GameTime.zero (GameTime.ofSeconds 1.0f); Body = Particles.Body.defaultBody; Size = Constants.Engine.ParticleSize2dDefault; Offset = v3Zero; Inset = box2Zero; Color = Color.One; Emission = Color.Zero; Flip = FlipNone }
             define Entity.EmitterConstraint Particles.Constraint.empty
             define Entity.EmitterStyle "BasicStaticSpriteEmitter"
             nonPersistent Entity.ParticleSystem Particles.ParticleSystem.empty]

        override this.Register (entity, world) =
            let emitter = makeEmitter entity world
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticSpriteEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            let world = entity.SetParticleSystem particleSystem world
            let world = World.sense handlePositionChange (entity.GetChangeEvent (nameof entity.Position)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleRotationChange (entity.GetChangeEvent (nameof entity.Rotation)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterBlendChange (entity.GetChangeEvent (nameof entity.EmitterBlend)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterImageChange (entity.GetChangeEvent (nameof entity.EmitterImage)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterLifeTimeOptChange (entity.GetChangeEvent (nameof entity.EmitterLifeTimeOpt)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleParticleLifeTimeMaxOptChange (entity.GetChangeEvent (nameof entity.ParticleLifeTimeMaxOpt)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleParticleRateChange (entity.GetChangeEvent (nameof entity.ParticleRate)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleParticleMaxChange (entity.GetChangeEvent (nameof entity.ParticleMax)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleBasicParticleSeedChange (entity.GetChangeEvent (nameof entity.BasicParticleSeed)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterConstraintChange (entity.GetChangeEvent (nameof entity.EmitterConstraint)) entity (nameof BasicStaticSpriteEmitterFacet) world
            let world = World.sense handleEmitterStyleChange (entity.GetChangeEvent (nameof entity.EmitterStyle)) entity (nameof BasicStaticSpriteEmitterFacet) world
            world

        override this.Unregister (entity, world) =
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.remove typeof<Particles.BasicStaticSpriteEmitter>.Name particleSystem.Emitters }
            entity.SetParticleSystem particleSystem world

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                let delta = world.GameDelta
                let time = world.GameTime
                let particleSystem = entity.GetParticleSystem world
                let (particleSystem, output) = Particles.ParticleSystem.run delta time particleSystem
                let world = entity.SetParticleSystem particleSystem world
                processOutput output entity world
            else world

        override this.Render (entity, world) =
            let time = world.GameTime
            let particleSystem = entity.GetParticleSystem world
            let particlesMessages =
                particleSystem |>
                Particles.ParticleSystem.toParticlesDescriptors time |>
                List.map (fun descriptor ->
                    match descriptor with
                    | Particles.SpriteParticlesDescriptor descriptor ->
                        Some
                            { Elevation = descriptor.Elevation
                              Horizon = descriptor.Horizon
                              AssetTag = AssetTag.generalize descriptor.Image
                              RenderOperation2d = RenderSpriteParticles descriptor }
                    | _ -> None) |>
                List.definitize
            World.enqueueLayeredOperations2d particlesMessages world

[<AutoOpen>]
module TextFacetModule =

    type Entity with
        member this.GetText world : string = this.Get (nameof this.Text) world
        member this.SetText (value : string) world = this.Set (nameof this.Text) value world
        member this.Text = lens (nameof this.Text) this this.GetText this.SetText
        member this.GetFont world : Font AssetTag = this.Get (nameof this.Font) world
        member this.SetFont (value : Font AssetTag) world = this.Set (nameof this.Font) value world
        member this.Font = lens (nameof this.Font) this this.GetFont this.SetFont
        member this.GetJustification world : Justification = this.Get (nameof this.Justification) world
        member this.SetJustification (value : Justification) world = this.Set (nameof this.Justification) value world
        member this.Justification = lens (nameof this.Justification) this this.GetJustification this.SetJustification
        member this.GetTextMargin world : Vector2 = this.Get (nameof this.TextMargin) world
        member this.SetTextMargin (value : Vector2) world = this.Set (nameof this.TextMargin) value world
        member this.TextMargin = lens (nameof this.TextMargin) this this.GetTextMargin this.SetTextMargin
        member this.GetTextColor world : Color = this.Get (nameof this.TextColor) world
        member this.SetTextColor (value : Color) world = this.Set (nameof this.TextColor) value world
        member this.TextColor = lens (nameof this.TextColor) this this.GetTextColor this.SetTextColor
        member this.GetTextDisabledColor world : Color = this.Get (nameof this.TextDisabledColor) world
        member this.SetTextDisabledColor (value : Color) world = this.Set (nameof this.TextDisabledColor) value world
        member this.TextDisabledColor = lens (nameof this.TextDisabledColor) this this.GetTextDisabledColor this.SetTextDisabledColor
        member this.GetTextOffset world : Vector2 = this.Get (nameof this.TextOffset) world
        member this.SetTextOffset (value : Vector2) world = this.Set (nameof this.TextOffset) value world
        member this.TextOffset = lens (nameof this.TextOffset) this this.GetTextOffset this.SetTextOffset
        member this.GetTextShift world : single = this.Get (nameof this.TextShift) world
        member this.SetTextShift (value : single) world = this.Set (nameof this.TextShift) value world
        member this.TextShift = lens (nameof this.TextShift) this this.GetTextShift this.SetTextShift

    /// Augments an entity with text.
    type TextFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Text ""
             define Entity.Font Assets.Default.Font
             define Entity.Justification (Justified (JustifyCenter, JustifyMiddle))
             define Entity.TextMargin v2Zero
             define Entity.TextColor Color.Black
             define Entity.TextDisabledColor (Color (0.25f, 0.25f, 0.25f, 0.75f))
             define Entity.TextOffset v2Zero
             define Entity.TextShift 0.5f]

        override this.Render (entity, world) =
            let text = entity.GetText world
            if not (String.IsNullOrWhiteSpace text) then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter // gui currently ignores rotation and scale
                let horizon = transform.Horizon
                let mutable textTransform = Transform.makeDefault false // centered-ness and offset are already baked into perimeter
                let margin = (entity.GetTextMargin world).V3
                let offset = (entity.GetTextOffset world).V3
                let shift = entity.GetTextShift world
                textTransform.Position <- perimeter.Min + margin + offset
                textTransform.Size <- perimeter.Size - margin * 2.0f
                textTransform.Elevation <- transform.Elevation + shift
                textTransform.Absolute <- transform.Absolute
                let font = entity.GetFont world
                World.enqueueLayeredOperation2d
                    { Elevation = textTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize font
                      RenderOperation2d =
                        RenderText
                            { Transform = textTransform
                              Text = text
                              Font = font
                              Color = if transform.Enabled then entity.GetTextColor world else entity.GetTextDisabledColor world
                              Justification = entity.GetJustification world }}
                    world
            else world

        override this.GetQuickSize (_, _) =
            Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module BackdroppableFacetModule =

    type Entity with
        member this.GetDisabledColor world : Color = this.Get (nameof this.DisabledColor) world
        member this.SetDisabledColor (value : Color) world = this.Set (nameof this.DisabledColor) value world
        member this.DisabledColor = lens (nameof this.DisabledColor) this this.GetDisabledColor this.SetDisabledColor
        member this.GetBackdropImageOpt world : Image AssetTag option = this.Get (nameof this.BackdropImageOpt) world
        member this.SetBackdropImageOpt (value : Image AssetTag option) world = this.Set (nameof this.BackdropImageOpt) value world
        member this.BackdropImageOpt = lens (nameof this.BackdropImageOpt) this this.GetBackdropImageOpt this.SetBackdropImageOpt

    /// Augments an entity with optional backdrop behavior.
    type BackdroppableFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f)) // TODO: make this a constant.
             define Entity.BackdropImageOpt None]

        override this.Render (entity, world) =
            match entity.GetBackdropImageOpt world with
            | Some spriteImage ->
                let mutable transform = entity.GetTransform world
                let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
                World.enqueueLayeredOperation2d
                    { Elevation = spriteTransform.Elevation
                      Horizon = spriteTransform.Horizon
                      AssetTag = AssetTag.generalize spriteImage
                      RenderOperation2d =
                        RenderSprite
                            { Transform = spriteTransform
                              InsetOpt = ValueNone
                              Image = spriteImage
                              Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                              Blend = Transparent
                              Emission = Color.Zero
                              Flip = FlipNone }}
                    world
            | None -> world

        override this.GetQuickSize (entity, world) =
            match entity.GetBackdropImageOpt world with
            | Some image ->
                match Metadata.tryGetTextureSizeF image with
                | Some size -> size.V3
                | None -> Constants.Engine.EntitySize2dDefault
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module LabelFacetModule =

    type Entity with
        member this.GetLabelImage world : Image AssetTag = this.Get (nameof this.LabelImage) world
        member this.SetLabelImage (value : Image AssetTag) world = this.Set (nameof this.LabelImage) value world
        member this.LabelImage = lens (nameof this.LabelImage) this this.GetLabelImage this.SetLabelImage

    /// Augments an entity with label behavior.
    type LabelFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.LabelImage Assets.Default.Label]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
            let spriteImage = entity.GetLabelImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetLabelImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module ButtonFacetModule =

    type Entity with
        member this.GetDown world : bool = this.Get (nameof this.Down) world
        member this.SetDown (value : bool) world = this.Set (nameof this.Down) value world
        member this.Down = lens (nameof this.Down) this this.GetDown this.SetDown
        member this.GetDownOffset world : Vector2 = this.Get (nameof this.DownOffset) world
        member this.SetDownOffset (value : Vector2) world = this.Set (nameof this.DownOffset) value world
        member this.DownOffset = lens (nameof this.DownOffset) this this.GetDownOffset this.SetDownOffset
        member this.GetUpImage world : Image AssetTag = this.Get (nameof this.UpImage) world
        member this.SetUpImage (value : Image AssetTag) world = this.Set (nameof this.UpImage) value world
        member this.UpImage = lens (nameof this.UpImage) this this.GetUpImage this.SetUpImage
        member this.GetDownImage world : Image AssetTag = this.Get (nameof this.DownImage) world
        member this.SetDownImage (value : Image AssetTag) world = this.Set (nameof this.DownImage) value world
        member this.DownImage = lens (nameof this.DownImage) this this.GetDownImage this.SetDownImage
        member this.GetClickSoundOpt world : Sound AssetTag option = this.Get (nameof this.ClickSoundOpt) world
        member this.SetClickSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.ClickSoundOpt) value world
        member this.ClickSoundOpt = lens (nameof this.ClickSoundOpt) this this.GetClickSoundOpt this.SetClickSoundOpt
        member this.GetClickSoundVolume world : single = this.Get (nameof this.ClickSoundVolume) world
        member this.SetClickSoundVolume (value : single) world = this.Set (nameof this.ClickSoundVolume) value world
        member this.ClickSoundVolume = lens (nameof this.ClickSoundVolume) this this.GetClickSoundVolume this.SetClickSoundVolume
        member this.UpEvent = Events.UpEvent --> this
        member this.DownEvent = Events.DownEvent --> this
        member this.ClickEvent = Events.ClickEvent --> this

    /// Augments an entity with button behavior.
    type ButtonFacet () =
        inherit Facet (false)

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetDown true world
                        let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) (entity.GetDownOffset world) world
                        let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftDown" "" EventTrace.empty
                        let world = World.publishPlus () entity.DownEvent eventTrace entity true false world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasDown = entity.GetDown world
            let world = entity.SetDown false world
            let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) v2Zero world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasDown then
                        let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftUp" "Up" EventTrace.empty
                        let world = World.publishPlus () entity.UpEvent eventTrace entity true false world
                        let eventTrace = EventTrace.debug "ButtonFacet" "handleMouseLeftUp" "Click" EventTrace.empty
                        let world = World.publishPlus () entity.ClickEvent eventTrace entity true false world
                        let world =
                            match entity.GetClickSoundOpt world with
                            | Some clickSound -> World.playSound (entity.GetClickSoundVolume world) clickSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.Down false
             define Entity.DownOffset v2Zero
             define Entity.UpImage Assets.Default.ButtonUp
             define Entity.DownImage Assets.Default.ButtonDown
             define Entity.ClickSoundOpt (Some Assets.Default.Sound)
             define Entity.ClickSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof ButtonFacet) world
            let world = World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof ButtonFacet) world
            world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
            let spriteImage = if entity.GetDown world then entity.GetDownImage world else entity.GetUpImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUpImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module ToggleButtonFacetModule =

    type Entity with
        member this.GetToggled world : bool = this.Get (nameof this.Toggled) world
        member this.SetToggled (value : bool) world = this.Set (nameof this.Toggled) value world
        member this.Toggled = lens (nameof this.Toggled) this this.GetToggled this.SetToggled
        member this.GetToggledOffset world : Vector2 = this.Get (nameof this.ToggledOffset) world
        member this.SetToggledOffset (value : Vector2) world = this.Set (nameof this.ToggledOffset) value world
        member this.ToggledOffset = lens (nameof this.ToggledOffset) this this.GetToggledOffset this.SetToggledOffset
        member this.GetPressed world : bool = this.Get (nameof this.Pressed) world
        member this.SetPressed (value : bool) world = this.Set (nameof this.Pressed) value world
        member this.Pressed = lens (nameof this.Pressed) this this.GetPressed this.SetPressed
        member this.GetPressedOffset world : Vector2 = this.Get (nameof this.PressedOffset) world
        member this.SetPressedOffset (value : Vector2) world = this.Set (nameof this.PressedOffset) value world
        member this.PressedOffset = lens (nameof this.PressedOffset) this this.GetPressedOffset this.SetPressedOffset
        member this.GetUntoggledImage world : Image AssetTag = this.Get (nameof this.UntoggledImage) world
        member this.SetUntoggledImage (value : Image AssetTag) world = this.Set (nameof this.UntoggledImage) value world
        member this.UntoggledImage = lens (nameof this.UntoggledImage) this this.GetUntoggledImage this.SetUntoggledImage
        member this.GetToggledImage world : Image AssetTag = this.Get (nameof this.ToggledImage) world
        member this.SetToggledImage (value : Image AssetTag) world = this.Set (nameof this.ToggledImage) value world
        member this.ToggledImage = lens (nameof this.ToggledImage) this this.GetToggledImage this.SetToggledImage
        member this.GetToggleSoundOpt world : Sound AssetTag option = this.Get (nameof this.ToggleSoundOpt) world
        member this.SetToggleSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.ToggleSoundOpt) value world
        member this.ToggleSoundOpt = lens (nameof this.ToggleSoundOpt) this this.GetToggleSoundOpt this.SetToggleSoundOpt
        member this.GetToggleSoundVolume world : single = this.Get (nameof this.ToggleSoundVolume) world
        member this.SetToggleSoundVolume (value : single) world = this.Set (nameof this.ToggleSoundVolume) value world
        member this.ToggleSoundVolume = lens (nameof this.ToggleSoundVolume) this this.GetToggleSoundVolume this.SetToggleSoundVolume
        member this.ToggleEvent = Events.ToggleEvent --> this
        member this.ToggledEvent = Events.ToggledEvent --> this
        member this.UntoggledEvent = Events.UntoggledEvent --> this

    /// Augments an entity with toggle button behavior.
    type ToggleButtonFacet () =
        inherit Facet (false)
        
        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasPressed then
                        let world = entity.SetToggled (not (entity.GetToggled world)) world
                        let toggled = entity.GetToggled world
                        let eventAddress = if toggled then entity.ToggledEvent else entity.UntoggledEvent
                        let eventTrace = EventTrace.debug "ToggleFacet" "handleMouseLeftUp" "" EventTrace.empty
                        let world = World.publishPlus () eventAddress eventTrace entity true false world
                        let eventTrace = EventTrace.debug "ToggleFacet" "handleMouseLeftUp" "Toggle" EventTrace.empty
                        let world = World.publishPlus toggled entity.ToggleEvent eventTrace entity true false world
                        let world =
                            match entity.GetToggleSoundOpt world with
                            | Some toggleSound -> World.playSound (entity.GetToggleSoundVolume world) toggleSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.Toggled false
             define Entity.ToggledOffset v2Zero
             define Entity.Pressed false
             define Entity.PressedOffset v2Zero
             define Entity.UntoggledImage Assets.Default.ButtonUp
             define Entity.ToggledImage Assets.Default.ButtonDown
             define Entity.ToggleSoundOpt (Some Assets.Default.Sound)
             define Entity.ToggleSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof ToggleButtonFacet) world
            let world = World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof ToggleButtonFacet) world
            world

        override this.Update (entity, world) =
            let textOffset =
                if entity.GetPressed world then entity.GetPressedOffset world
                elif entity.GetToggled world then entity.GetToggledOffset world
                else v2Zero
            let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) textOffset world
            world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
            let spriteImage =
                if entity.GetToggled world || entity.GetPressed world
                then entity.GetToggledImage world
                else entity.GetUntoggledImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUntoggledImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module RadioButtonFacetModule =

    type Entity with
        member this.GetDialed world : bool = this.Get (nameof this.Dialed) world
        member this.SetDialed (value : bool) world = this.Set (nameof this.Dialed) value world
        member this.Dialed = lens (nameof this.Dialed) this this.GetDialed this.SetDialed
        member this.GetDialedOffset world : Vector2 = this.Get (nameof this.DialedOffset) world
        member this.SetDialedOffset (value : Vector2) world = this.Set (nameof this.DialedOffset) value world
        member this.DialedOffset = lens (nameof this.DialedOffset) this this.GetDialedOffset this.SetDialedOffset
        member this.GetUndialedImage world : Image AssetTag = this.Get (nameof this.UndialedImage) world
        member this.SetUndialedImage (value : Image AssetTag) world = this.Set (nameof this.UndialedImage) value world
        member this.UndialedImage = lens (nameof this.UndialedImage) this this.GetUndialedImage this.SetUndialedImage
        member this.GetDialedImage world : Image AssetTag = this.Get (nameof this.DialedImage) world
        member this.SetDialedImage (value : Image AssetTag) world = this.Set (nameof this.DialedImage) value world
        member this.DialedImage = lens (nameof this.DialedImage) this this.GetDialedImage this.SetDialedImage
        member this.GetDialSoundOpt world : Sound AssetTag option = this.Get (nameof this.DialSoundOpt) world
        member this.SetDialSoundOpt (value : Sound AssetTag option) world = this.Set (nameof this.DialSoundOpt) value world
        member this.DialSoundOpt = lens (nameof this.DialSoundOpt) this this.GetDialSoundOpt this.SetDialSoundOpt
        member this.GetDialSoundVolume world : single = this.Get (nameof this.DialSoundVolume) world
        member this.SetDialSoundVolume (value : single) world = this.Set (nameof this.DialSoundVolume) value world
        member this.DialSoundVolume = lens (nameof this.DialSoundVolume) this this.GetDialSoundVolume this.SetDialSoundVolume
        member this.DialEvent = Events.DialEvent --> this
        member this.DialedEvent = Events.DialedEvent --> this
        member this.UndialedEvent = Events.UndialedEvent --> this

    /// Augments an entity with radio button behavior.
    type RadioButtonFacet () =
        inherit Facet (false)

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetPressed true world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let wasPressed = entity.GetPressed world
            let world = if wasPressed then entity.SetPressed false world else world
            let wasDialed = entity.GetDialed world
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled && wasPressed && not wasDialed then
                        let world = entity.SetDialed true world
                        let dialed = entity.GetDialed world
                        let eventAddress = if dialed then entity.DialedEvent else entity.UndialedEvent
                        let eventTrace = EventTrace.debug "RadioButtonFacet" "handleMouseLeftUp" "" EventTrace.empty
                        let world = World.publishPlus () eventAddress eventTrace entity true false world
                        let eventTrace = EventTrace.debug "RadioButtonFacet" "handleMouseLeftUp" "Dial" EventTrace.empty
                        let world = World.publishPlus dialed entity.DialEvent eventTrace entity true false world
                        let world =
                            match entity.GetDialSoundOpt world with
                            | Some dialSound -> World.playSound (entity.GetDialSoundVolume world) dialSound world
                            | None -> world
                        (Resolve, world)
                    else (Cascade, world)
                else (Cascade, world)
            else (Cascade, world)

        static member Properties =
            [define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.Dialed false
             define Entity.DialedOffset v2Zero
             define Entity.Pressed false
             define Entity.PressedOffset v2Zero
             define Entity.UndialedImage Assets.Default.ButtonUp
             define Entity.DialedImage Assets.Default.ButtonDown
             define Entity.DialSoundOpt (Some Assets.Default.Sound)
             define Entity.DialSoundVolume Constants.Audio.SoundVolumeDefault]

        override this.Register (entity, world) =
            let world = World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof RadioButtonFacet) world
            let world = World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof RadioButtonFacet) world
            world

        override this.Update (entity, world) =
            let textOffset =
                if entity.GetPressed world then entity.GetPressedOffset world
                elif entity.GetDialed world then entity.GetDialedOffset world
                else v2Zero
            let struct (_, _, world) = entity.TrySet (nameof Entity.TextOffset) textOffset world
            world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let mutable spriteTransform = Transform.makePerimeter transform.Perimeter transform.Offset transform.Elevation transform.Absolute transform.Centered
            let spriteImage =
                if entity.GetDialed world || entity.GetPressed world
                then entity.GetDialedImage world
                else entity.GetUndialedImage world
            World.enqueueLayeredOperation2d
                { Elevation = spriteTransform.Elevation
                  Horizon = spriteTransform.Horizon
                  AssetTag = AssetTag.generalize spriteImage
                  RenderOperation2d =
                    RenderSprite
                        { Transform = spriteTransform
                          InsetOpt = ValueNone
                          Image = spriteImage
                          Color = if transform.Enabled then Color.One else entity.GetDisabledColor world
                          Blend = Transparent
                          Emission = Color.Zero
                          Flip = FlipNone }}
                world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetUndialedImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module FillBarFacetModule =

    type Entity with
        member this.GetFill world : single = this.Get (nameof this.Fill) world
        member this.SetFill (value : single) world = this.Set (nameof this.Fill) value world
        member this.Fill = lens (nameof this.Fill) this this.GetFill this.SetFill
        member this.GetFillInset world : single = this.Get (nameof this.FillInset) world
        member this.SetFillInset (value : single) world = this.Set (nameof this.FillInset) value world
        member this.FillInset = lens (nameof this.FillInset) this this.GetFillInset this.SetFillInset
        member this.GetFillColor world : Color = this.Get (nameof this.FillColor) world
        member this.SetFillColor (value : Color) world = this.Set (nameof this.FillColor) value world
        member this.FillColor = lens (nameof this.FillColor) this this.GetFillColor this.SetFillColor
        member this.GetFillImage world : Image AssetTag = this.Get (nameof this.FillImage) world
        member this.SetFillImage (value : Image AssetTag) world = this.Set (nameof this.FillImage) value world
        member this.FillImage = lens (nameof this.FillImage) this this.GetFillImage this.SetFillImage
        member this.GetBorderColor world : Color = this.Get (nameof this.BorderColor) world
        member this.SetBorderColor (value : Color) world = this.Set (nameof this.BorderColor) value world
        member this.BorderColor = lens (nameof this.BorderColor) this this.GetBorderColor this.SetBorderColor
        member this.GetBorderImage world : Image AssetTag = this.Get (nameof this.BorderImage) world
        member this.SetBorderImage (value : Image AssetTag) world = this.Set (nameof this.BorderImage) value world
        member this.BorderImage = lens (nameof this.BorderImage) this this.GetBorderImage this.SetBorderImage

    /// Augments an entity with fill bar behavior.
    type FillBarFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.DisabledColor (Color (0.75f, 0.75f, 0.75f, 0.75f))
             define Entity.Fill 0.0f
             define Entity.FillInset 0.0f
             define Entity.FillColor (Color (1.0f, 0.0f, 0.0f, 1.0f))
             define Entity.FillImage Assets.Default.White
             define Entity.BorderColor (Color (0.0f, 0.0f, 0.0f, 1.0f))
             define Entity.BorderImage Assets.Default.Border]

        override this.Render (entity, world) =

            // border sprite
            let mutable transform = entity.GetTransform world
            let perimeter = transform.Perimeter // gui currently ignores rotation
            let horizon = transform.Horizon
            let mutable borderTransform = Transform.makeDefault transform.Centered
            borderTransform.Position <- perimeter.Min
            borderTransform.Size <- perimeter.Size
            borderTransform.Offset <- transform.Offset
            borderTransform.Elevation <- transform.Elevation + 0.5f
            borderTransform.Absolute <- transform.Absolute
            let color = if transform.Enabled then Color.White else entity.GetDisabledColor world
            let borderImageColor = entity.GetBorderColor world * color
            let borderImage = entity.GetBorderImage world
            let world =
                World.enqueueLayeredOperation2d
                    { Elevation = borderTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize borderImage
                      RenderOperation2d =
                        RenderSprite
                            { Transform = borderTransform
                              InsetOpt = ValueNone
                              Image = borderImage
                              Color = borderImageColor
                              Blend = Transparent
                              Emission = Color.Zero
                              Flip = FlipNone }}
                    world

            // fill sprite
            let fillSize = perimeter.Size
            let fillInset = fillSize.X * entity.GetFillInset world * 0.5f
            let fillPosition = perimeter.Min + v3 fillInset fillInset 0.0f
            let fillWidth = (fillSize.X - fillInset * 2.0f) * entity.GetFill world
            let fillHeight = fillSize.Y - fillInset * 2.0f
            let fillSize = v3 fillWidth fillHeight 0.0f
            let mutable fillTransform = Transform.makeDefault transform.Centered
            fillTransform.Position <- fillPosition
            fillTransform.Size <- fillSize
            fillTransform.Offset <- transform.Offset
            fillTransform.Elevation <- transform.Elevation
            fillTransform.Absolute <- transform.Absolute
            let fillImageColor = entity.GetFillColor world * color
            let fillImage = entity.GetFillImage world
            let world =
                World.enqueueLayeredOperation2d
                    { Elevation = fillTransform.Elevation
                      Horizon = horizon
                      AssetTag = AssetTag.generalize fillImage
                      RenderOperation2d =
                          RenderSprite
                              { Transform = fillTransform
                                InsetOpt = ValueNone
                                Image = fillImage
                                Color = fillImageColor
                                Blend = Transparent
                                Emission = Color.Zero
                                Flip = FlipNone }}
                    world

            // fin
            world

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetTextureSizeF (entity.GetBorderImage world) with
            | Some size -> size.V3
            | None -> Constants.Engine.EntitySizeGuiDefault

[<AutoOpen>]
module FeelerFacetModule =

    type Entity with
        member this.GetTouched world : bool = this.Get (nameof this.Touched) world
        member this.SetTouched (value : bool) world = this.Set (nameof this.Touched) value world
        member this.Touched = lens (nameof this.Touched) this this.GetTouched this.SetTouched
        member this.TouchEvent = Events.TouchEvent --> this
        member this.TouchingEvent = Events.TouchingEvent --> this
        member this.UntouchEvent = Events.UntouchEvent --> this

    /// Augments an entity with feeler behavior.
    type FeelerFacet () =
        inherit Facet (false)

        static let handleMouseLeftDown evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            if entity.GetVisible world then
                let mutable transform = entity.GetTransform world
                let perimeter = transform.Perimeter.Box2
                let mousePositionWorld = World.getMousePostion2dWorld transform.Absolute world
                if perimeter.Intersects mousePositionWorld then // gui currently ignores rotation
                    if transform.Enabled then
                        let world = entity.SetTouched true world
                        let eventTrace = EventTrace.debug "FeelerFacet" "handleMouseLeftDown" "" EventTrace.empty
                        let world = World.publishPlus data.Position entity.TouchEvent eventTrace entity true false world
                        (Resolve, world)
                    else (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleMouseLeftUp evt world =
            let entity = evt.Subscriber : Entity
            let data = evt.Data : MouseButtonData
            let wasTouched = entity.GetTouched world
            let world = entity.SetTouched false world
            if entity.GetVisible world then
                if entity.GetEnabled world && wasTouched then
                    let eventTrace = EventTrace.debug "FeelerFacet" "handleMouseLeftDown" "" EventTrace.empty
                    let world = World.publishPlus data.Position entity.UntouchEvent eventTrace entity true false world
                    (Resolve, world)
                else (Cascade, world)
            else (Cascade, world)

        static let handleIncoming evt world =
            let entity = evt.Subscriber : Entity
            if  MouseState.isButtonDown MouseLeft &&
                entity.GetVisible world &&
                entity.GetEnabled world then
                let mousePosition = MouseState.getPosition ()
                let world = entity.SetTouched true world
                let eventTrace = EventTrace.debug "FeelerFacet" "handleIncoming" "" EventTrace.empty
                let world = World.publishPlus mousePosition entity.TouchEvent eventTrace entity true false world
                (Resolve, world)
            else (Cascade, world)

        static let handleOutgoing evt world =
            let entity = evt.Subscriber : Entity
            (Cascade, entity.SetTouched false world)

        static member Properties =
            [define Entity.Touched false]

        override this.Register (entity, world) =
            let world = World.sense handleMouseLeftDown Nu.Game.Handle.MouseLeftDownEvent entity (nameof FeelerFacet) world
            let world = World.sense handleMouseLeftUp Nu.Game.Handle.MouseLeftUpEvent entity (nameof FeelerFacet) world
            let world = World.sense handleIncoming entity.Screen.IncomingFinishEvent entity (nameof FeelerFacet) world
            let world = World.sense handleOutgoing entity.Screen.OutgoingStartEvent entity (nameof FeelerFacet) world
            world

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                if entity.GetTouched world then
                    let mousePosition = World.getMousePosition world
                    let eventTrace = EventTrace.debug "FeelerFacet" "Update" "" EventTrace.empty
                    let world = World.publishPlus mousePosition entity.TouchingEvent eventTrace entity true false world
                    world
                else world
            else world

        override this.GetQuickSize (_, _) =
            Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module EffectFacetModule =

    /// The timing with which an effect should be evaluated in a frame.
    type RunMode =
        | RunEarly
        | RunLate

    type Entity with

        member this.GetRunMode world : RunMode = this.Get (nameof this.RunMode) world
        member this.SetRunMode (value : RunMode) world = this.Set (nameof this.RunMode) value world
        member this.RunMode = lens (nameof this.RunMode) this this.GetRunMode this.SetRunMode
        member this.GetEffectSymbolOpt world : Symbol AssetTag option = this.Get (nameof this.EffectSymbolOpt) world
        member this.SetEffectSymbolOpt (value : Symbol AssetTag option) world = this.Set (nameof this.EffectSymbolOpt) value world
        member this.EffectSymbolOpt = lens (nameof this.EffectSymbolOpt) this this.GetEffectSymbolOpt this.SetEffectSymbolOpt
        member this.GetEffectStartTimeOpt world : GameTime option = this.Get (nameof this.EffectStartTimeOpt) world
        member this.SetEffectStartTimeOpt (value : GameTime option) world = this.Set (nameof this.EffectStartTimeOpt) value world
        member this.EffectStartTimeOpt = lens (nameof this.EffectStartTimeOpt) this this.GetEffectStartTimeOpt this.SetEffectStartTimeOpt
        member this.GetEffectDefinitions world : Effects.Definitions = this.Get (nameof this.EffectDefinitions) world
        member this.SetEffectDefinitions (value : Effects.Definitions) world = this.Set (nameof this.EffectDefinitions) value world
        member this.EffectDefinitions = lens (nameof this.EffectDefinitions) this this.GetEffectDefinitions this.SetEffectDefinitions
        member this.GetEffectDescriptor world : Effects.EffectDescriptor = this.Get (nameof this.EffectDescriptor) world
        /// When RunMode is set to RunEarly, call this AFTER setting the rest of the entity's properties. This
        /// is because setting the effect descriptin in RunEarly mode will immediately run the first frame of the
        /// effect due to a semantic limitation in Nu.
        member this.SetEffectDescriptor (value : Effects.EffectDescriptor) world = this.Set (nameof this.EffectDescriptor) value world
        member this.EffectDescriptor = lens (nameof this.EffectDescriptor) this this.GetEffectDescriptor this.SetEffectDescriptor
        member this.GetEffectCentered world : bool = this.Get (nameof this.EffectCentered) world
        member this.SetEffectCentered (value : bool) world = this.Set (nameof this.EffectCentered) value world
        member this.EffectCentered = lens (nameof this.EffectCentered) this this.GetEffectCentered this.SetEffectCentered
        member this.GetEffectOffset world : Vector3 = this.Get (nameof this.EffectOffset) world
        member this.SetEffectOffset (value : Vector3) world = this.Set (nameof this.EffectOffset) value world
        member this.EffectOffset = lens (nameof this.EffectOffset) this this.GetEffectOffset this.SetEffectOffset
        member this.GetEffectRenderType world : RenderType = this.Get (nameof this.EffectRenderType) world
        member this.SetEffectRenderType (value : RenderType) world = this.Set (nameof this.EffectRenderType) value world
        member this.EffectRenderType = lens (nameof this.EffectRenderType) this this.GetEffectRenderType this.SetEffectRenderType
        member this.GetEffectHistoryMax world : int = this.Get (nameof this.EffectHistoryMax) world
        member this.SetEffectHistoryMax (value : int) world = this.Set (nameof this.EffectHistoryMax) value world
        member this.EffectHistoryMax = lens (nameof this.EffectHistoryMax) this this.GetEffectHistoryMax this.SetEffectHistoryMax
        member this.GetEffectHistory world : Effects.Slice Deque = this.Get (nameof this.EffectHistory) world
        member this.EffectHistory = lensReadOnly (nameof this.EffectHistory) this this.GetEffectHistory
        member this.GetEffectTags world : Map<string, Effects.Slice> = this.Get (nameof this.EffectTags) world
        member this.SetEffectTags (value : Map<string, Effects.Slice>) world = this.Set (nameof this.EffectTags) value world
        member this.EffectTags = lens (nameof this.EffectTags) this this.GetEffectTags this.SetEffectTags

        /// The start time of the effect, or zero if none.
        member this.GetEffectStartTime world =
            match this.GetEffectStartTimeOpt world with
            | Some effectStartTime -> effectStartTime
            | None -> GameTime.zero

    /// Augments an entity with an effect.
    type EffectFacet () =
        inherit Facet (false)

        static let setEffect effectSymbolOpt (entity : Entity) world =
            match effectSymbolOpt with
            | Some effectSymbol ->
                let symbolLoadMetadata = { ImplicitDelimiters = false; StripCsvHeader = false }
                match World.assetTagToValueOpt<Effects.EffectDescriptor> effectSymbol symbolLoadMetadata world with
                | Some effect -> entity.SetEffectDescriptor effect world
                | None -> world
            | None -> world

        static let run (entity : Entity) world =

            // make effect
            let effect =
                Effect.makePlus
                    (entity.GetEffectStartTime world)
                    (entity.GetEffectCentered world)
                    (entity.GetEffectOffset world)
                    (entity.GetTransform world)
                    (entity.GetEffectRenderType world)
                    (entity.GetParticleSystem world)
                    (entity.GetEffectHistoryMax world)
                    (entity.GetEffectHistory world)
                    (entity.GetEffectDefinitions world)
                    (entity.GetEffectDescriptor world)

            // run effect, optionally destroying upon exhaustion
            let (liveness, effect, world) = Effect.run effect world
            let world = entity.SetParticleSystem effect.ParticleSystem world
            let world = entity.SetEffectTags effect.Tags world
            if liveness = Dead && entity.GetSelfDestruct world
            then World.destroyEntity entity world
            else world

        static let handleEffectDescriptorChange evt world =
            let entity = evt.Subscriber : Entity
            let world =
                if entity.GetEnabled world then
                    match entity.GetRunMode world with
                    | RunEarly -> run entity world
                    | _ -> world
                else world
            (Cascade, world)

        static let handleEffectsChange evt world =
            let entity = evt.Subscriber : Entity
            let world = setEffect (entity.GetEffectSymbolOpt world) entity world
            (Cascade, world)

        static let handleAssetsReload evt world =
            let entity = evt.Subscriber : Entity
            let world = setEffect (entity.GetEffectSymbolOpt world) entity world
            (Cascade, world)

#if DISABLE_ENTITY_PRE_UPDATE
        static let handlePreUpdate evt world =
            let entity = evt.Subscriber : Entity
            let world =
                if entity.GetEnabled world then
                    match entity.GetRunMode world with
                    | RunEarly -> run entity world
                    | _ -> world
                else world
            (Cascade, world)
#endif

#if DISABLE_ENTITY_POST_UPDATE
        static let handlePostUpdate evt world =
            let entity = evt.Subscriber : Entity
            let world =
                if entity.GetEnabled world then
                    match entity.GetRunMode world with
                    | RunLate -> run entity world
                    | _ -> world
                else world
            (Cascade, world)
#endif

        static member Properties =
            [define Entity.ParticleSystem Particles.ParticleSystem.empty
             define Entity.SelfDestruct false
             define Entity.RunMode RunLate
             define Entity.EffectSymbolOpt None
             define Entity.EffectStartTimeOpt None
             define Entity.EffectDefinitions Map.empty
             define Entity.EffectDescriptor Effects.EffectDescriptor.empty
             define Entity.EffectCentered true
             define Entity.EffectOffset v3Zero
             define Entity.EffectRenderType (ForwardRenderType (0.0f, 0.0f))
             define Entity.EffectHistoryMax Constants.Effects.EffectHistoryMaxDefault
             variable Entity.EffectHistory (fun _ -> Deque<Effects.Slice> (inc Constants.Effects.EffectHistoryMaxDefault))
             nonPersistent Entity.EffectTags Map.empty]

        override this.Register (entity, world) =
            let effectStartTime = Option.defaultValue world.GameTime (entity.GetEffectStartTimeOpt world)
            let world = entity.SetEffectStartTimeOpt (Some effectStartTime) world
            let world = World.sense handleEffectDescriptorChange (entity.GetChangeEvent (nameof entity.EffectDescriptor)) entity (nameof EffectFacet) world
            let world = World.sense handleEffectsChange (entity.GetChangeEvent (nameof entity.EffectSymbolOpt)) entity (nameof EffectFacet) world
            let world = World.sense handleAssetsReload Nu.Game.Handle.AssetsReloadEvent entity (nameof EffectFacet) world
#if DISABLE_ENTITY_PRE_UPDATE
            let world = World.sense handlePreUpdate entity.Group.PreUpdateEvent entity (nameof EffectFacet) world
#endif
#if DISABLE_ENTITY_POST_UPDATE
            let world = World.sense handlePostUpdate entity.Group.PostUpdateEvent entity (nameof EffectFacet) world
#endif
            world

#if !DISABLE_ENTITY_PRE_UPDATE
        override this.PreUpdate (entity, world) =
            if entity.GetEnabled world && entity.GetRunMode world = RunEarly
            then run entity world
            else world
#endif

#if !DISABLE_ENTITY_POST_UPDATE
        override this.PostUpdate (entity, world) =
            if entity.GetEnabled world && entity.GetRunMode world = RunLate
            then run entity world
            else world
#endif

        override this.RayCast (ray, entity, world) =
            if entity.GetIs3d world then
                let intersectionOpt = ray.Intersects (entity.GetBounds world)
                if intersectionOpt.HasValue then [|intersectionOpt.Value|]
                else [||]
            else [||]

[<AutoOpen>]
module RigidBodyFacetModule =

    type Entity with
        member this.GetBodyEnabled world : bool = this.Get (nameof this.BodyEnabled) world
        member this.SetBodyEnabled (value : bool) world = this.Set (nameof this.BodyEnabled) value world
        member this.BodyEnabled = lens (nameof this.BodyEnabled) this this.GetBodyEnabled this.SetBodyEnabled
        member this.GetBodyType world : BodyType = this.Get (nameof this.BodyType) world
        member this.SetBodyType (value : BodyType) world = this.Set (nameof this.BodyType) value world
        member this.BodyType = lens (nameof this.BodyType) this this.GetBodyType this.SetBodyType
        member this.GetSleepingAllowed world : bool = this.Get (nameof this.SleepingAllowed) world
        member this.SetSleepingAllowed (value : bool) world = this.Set (nameof this.SleepingAllowed) value world
        member this.SleepingAllowed = lens (nameof this.SleepingAllowed) this this.GetSleepingAllowed this.SetSleepingAllowed
        member this.GetFriction world : single = this.Get (nameof this.Friction) world
        member this.SetFriction (value : single) world = this.Set (nameof this.Friction) value world
        member this.Friction = lens (nameof this.Friction) this this.GetFriction this.SetFriction
        member this.GetRestitution world : single = this.Get (nameof this.Restitution) world
        member this.SetRestitution (value : single) world = this.Set (nameof this.Restitution) value world
        member this.Restitution = lens (nameof this.Restitution) this this.GetRestitution this.SetRestitution
        member this.GetLinearVelocity world : Vector3 = this.Get (nameof this.LinearVelocity) world
        member this.SetLinearVelocity (value : Vector3) world = this.Set (nameof this.LinearVelocity) value world
        member this.LinearVelocity = lens (nameof this.LinearVelocity) this this.GetLinearVelocity this.SetLinearVelocity
        member this.GetLinearDamping world : single = this.Get (nameof this.LinearDamping) world
        member this.SetLinearDamping (value : single) world = this.Set (nameof this.LinearDamping) value world
        member this.LinearDamping = lens (nameof this.LinearDamping) this this.GetLinearDamping this.SetLinearDamping
        member this.GetAngularVelocity world : Vector3 = this.Get (nameof this.AngularVelocity) world
        member this.SetAngularVelocity (value : Vector3) world = this.Set (nameof this.AngularVelocity) value world
        member this.AngularVelocity = lens (nameof this.AngularVelocity) this this.GetAngularVelocity this.SetAngularVelocity
        member this.GetAngularDamping world : single = this.Get (nameof this.AngularDamping) world
        member this.SetAngularDamping (value : single) world = this.Set (nameof this.AngularDamping) value world
        member this.AngularDamping = lens (nameof this.AngularDamping) this this.GetAngularDamping this.SetAngularDamping
        member this.GetAngularFactor world : Vector3 = this.Get (nameof this.AngularFactor) world
        member this.SetAngularFactor (value : Vector3) world = this.Set (nameof this.AngularFactor) value world
        member this.AngularFactor = lens (nameof this.AngularFactor) this this.GetAngularFactor this.SetAngularFactor
        member this.GetSubstance world : Substance = this.Get (nameof this.Substance) world
        member this.SetSubstance (value : Substance) world = this.Set (nameof this.Substance) value world
        member this.Substance = lens (nameof this.Substance) this this.GetSubstance this.SetSubstance
        member this.GetGravityOverride world : Vector3 option = this.Get (nameof this.GravityOverride) world
        member this.SetGravityOverride (value : Vector3 option) world = this.Set (nameof this.GravityOverride) value world
        member this.GravityOverride = lens (nameof this.GravityOverride) this this.GetGravityOverride this.SetGravityOverride
        member this.GetCollisionDetection world : CollisionDetection = this.Get (nameof this.CollisionDetection) world
        member this.SetCollisionDetection (value : CollisionDetection) world = this.Set (nameof this.CollisionDetection) value world
        member this.CollisionDetection = lens (nameof this.CollisionDetection) this this.GetCollisionDetection this.SetCollisionDetection
        member this.GetCollisionCategories world : string = this.Get (nameof this.CollisionCategories) world
        member this.SetCollisionCategories (value : string) world = this.Set (nameof this.CollisionCategories) value world
        member this.CollisionCategories = lens (nameof this.CollisionCategories) this this.GetCollisionCategories this.SetCollisionCategories
        member this.GetCollisionMask world : string = this.Get (nameof this.CollisionMask) world
        member this.SetCollisionMask (value : string) world = this.Set (nameof this.CollisionMask) value world
        member this.CollisionMask = lens (nameof this.CollisionMask) this this.GetCollisionMask this.SetCollisionMask
        member this.GetBodyShape world : BodyShape = this.Get (nameof this.BodyShape) world
        member this.SetBodyShape (value : BodyShape) world = this.Set (nameof this.BodyShape) value world
        member this.BodyShape = lens (nameof this.BodyShape) this this.GetBodyShape this.SetBodyShape
        member this.GetSensor world : bool = this.Get (nameof this.Sensor) world
        member this.SetSensor (value : bool) world = this.Set (nameof this.Sensor) value world
        member this.Sensor = lens (nameof this.Sensor) this this.GetSensor this.SetSensor
        member this.GetModelDriven world : bool = this.Get (nameof this.ModelDriven) world
        member this.SetModelDriven (value : bool) world = this.Set (nameof this.ModelDriven) value world
        member this.ModelDriven = lens (nameof this.ModelDriven) this this.GetModelDriven this.SetModelDriven
        member this.GetBodyId world : BodyId = this.Get (nameof this.BodyId) world
        member this.BodyId = lensReadOnly (nameof this.BodyId) this this.GetBodyId
        member this.BodyCollisionEvent = Events.BodyCollisionEvent --> this
        member this.BodySeparationImplicitEvent = Events.BodySeparationImplicitEvent --> Game
        member this.BodySeparationExplicitEvent = Events.BodySeparationExplicitEvent --> this
        member this.BodyTransformEvent = Events.BodyTransformEvent --> this

    /// Augments an entity with a physics-driven rigid body.
    type RigidBodyFacet () =
        inherit Facet (true)

        static let getBodyShape (entity : Entity) world =
            let scalar = entity.GetScale world * entity.GetSize world
            let bodyShape = entity.GetBodyShape world
            if entity.GetIs2d world
            then World.localizeBodyShape scalar bodyShape world
            else bodyShape

        static member Properties =
            [define Entity.BodyEnabled true
             define Entity.BodyType Dynamic
             define Entity.SleepingAllowed true
             define Entity.Friction 0.2f
             define Entity.Restitution 0.0f
             define Entity.LinearVelocity v3Zero
             define Entity.LinearDamping 0.0f // leave this up to friction by default
             define Entity.AngularVelocity v3Zero
             define Entity.AngularDamping 0.2f
             define Entity.AngularFactor v3One
             define Entity.Substance (Mass 1.0f)
             define Entity.GravityOverride None
             define Entity.CollisionDetection Discontinuous
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.BodyShape (BodyBox { Size = v3One; TransformOpt = None; PropertiesOpt = None })
             define Entity.Sensor false
             define Entity.ModelDriven false
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = Constants.Physics.InternalIndex }) None]

        override this.Register (entity, world) =

            // OPTIMIZATION: using manual unsubscription in order to use less live objects for subscriptions.
            let subIds = Array.init 24 (fun _ -> makeGuid ())
            let (_, world) = World.subscribePlus subIds.[0] (fun _ world -> (Cascade, if not (entity.GetModelDriven world) then entity.PropagatePhysics world else world)) (entity.ChangeEvent (nameof entity.Position)) entity world
            let (_, world) = World.subscribePlus subIds.[1] (fun _ world -> (Cascade, if not (entity.GetModelDriven world) then entity.PropagatePhysics world else world)) (entity.ChangeEvent (nameof entity.Rotation)) entity world
            let (_, world) = World.subscribePlus subIds.[2] (fun _ world -> (Cascade, if not (entity.GetModelDriven world) then entity.PropagatePhysics world else world)) (entity.ChangeEvent (nameof entity.LinearVelocity)) entity world
            let (_, world) = World.subscribePlus subIds.[3] (fun _ world -> (Cascade, if not (entity.GetModelDriven world) then entity.PropagatePhysics world else world)) (entity.ChangeEvent (nameof entity.AngularVelocity)) entity world
            let (_, world) = World.subscribePlus subIds.[4] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Scale)) entity world
            let (_, world) = World.subscribePlus subIds.[5] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Offset)) entity world
            let (_, world) = World.subscribePlus subIds.[6] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Size)) entity world
            let (_, world) = World.subscribePlus subIds.[7] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Centered)) entity world
            let (_, world) = World.subscribePlus subIds.[8] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity world
            let (_, world) = World.subscribePlus subIds.[9] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyType)) entity world
            let (_, world) = World.subscribePlus subIds.[10] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.SleepingAllowed)) entity world
            let (_, world) = World.subscribePlus subIds.[11] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity world
            let (_, world) = World.subscribePlus subIds.[12] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity world
            let (_, world) = World.subscribePlus subIds.[13] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.LinearDamping)) entity world
            let (_, world) = World.subscribePlus subIds.[14] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.AngularDamping)) entity world
            let (_, world) = World.subscribePlus subIds.[15] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.AngularFactor)) entity world
            let (_, world) = World.subscribePlus subIds.[16] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Substance)) entity world
            let (_, world) = World.subscribePlus subIds.[17] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.GravityOverride)) entity world
            let (_, world) = World.subscribePlus subIds.[18] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionDetection)) entity world
            let (_, world) = World.subscribePlus subIds.[19] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity world
            let (_, world) = World.subscribePlus subIds.[20] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity world
            let (_, world) = World.subscribePlus subIds.[21] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyShape)) entity world
            let (_, world) = World.subscribePlus subIds.[23] (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Sensor)) entity world
            let unsubscribe = fun world ->
                Array.fold (fun world subId -> World.unsubscribe subId world) world subIds
            let callback = fun evt world ->
                if  Set.contains (nameof RigidBodyFacet) (evt.Data.Previous :?> string Set) &&
                    not (Set.contains (nameof RigidBodyFacet) (evt.Data.Value :?> string Set)) then
                    (Cascade, unsubscribe world)
                else (Cascade, world)
            let callback2 = fun _ world ->
                (Cascade, unsubscribe world)
            let world = World.sense callback entity.FacetNames.ChangeEvent entity (nameof RigidBodyFacet) world
            let world = World.sense callback2 entity.UnregisteringEvent entity (nameof RigidBodyFacet) world
            world

        override this.RegisterPhysics (entity, world) =
            let mutable transform = entity.GetTransform world
            let bodyProperties =
                { BodyIndex = (entity.GetBodyId world).BodyIndex
                  Center = transform.Center
                  Rotation = transform.Rotation
                  BodyShape = getBodyShape entity world
                  BodyType = entity.GetBodyType world
                  SleepingAllowed = entity.GetSleepingAllowed world
                  Enabled = entity.GetBodyEnabled world
                  Friction = entity.GetFriction world
                  Restitution = entity.GetRestitution world
                  LinearVelocity = entity.GetLinearVelocity world
                  LinearDamping = entity.GetLinearDamping world
                  AngularVelocity = entity.GetAngularVelocity world
                  AngularDamping = entity.GetAngularDamping world
                  AngularFactor = entity.GetAngularFactor world
                  Substance = entity.GetSubstance world
                  GravityOverride = entity.GetGravityOverride world
                  CollisionDetection = entity.GetCollisionDetection world
                  CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                  CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                  Sensor = entity.GetSensor world }
            let world = World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world
            let world = World.updateBodyObservable false entity world
            world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

[<AutoOpen>]
module JointFacetModule =

    type Entity with
        member this.GetJointDevice world : JointDevice = this.Get (nameof this.JointDevice) world
        member this.SetJointDevice (value : JointDevice) world = this.Set (nameof this.JointDevice) value world
        member this.JointDevice = lens (nameof this.JointDevice) this this.GetJointDevice this.SetJointDevice
        member this.GetJointId world : JointId = this.Get (nameof this.JointId) world
        member this.JointId = lensReadOnly (nameof this.JointId) this this.GetJointId

    /// Augments an entity with a physics-driven joint.
    type JointFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.JointDevice JointEmpty
             computed Entity.JointId (fun (entity : Entity) _ -> { JointSource = entity; JointIndex = Constants.Physics.InternalIndex }) None]

        override this.Register (entity, world) =
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof JointFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.JointDevice)) entity (nameof JointFacet) world
            world

        override this.RegisterPhysics (entity, world) =
            let jointProperties =
                { JointIndex = (entity.GetJointId world).JointIndex
                  JointDevice = (entity.GetJointDevice world) }
            World.createJoint (entity.GetIs2d world) entity jointProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyJoint (entity.GetIs2d world) (entity.GetJointId world) world

[<AutoOpen>]
module TileMapFacetModule =

    type Entity with
        member this.GetTileLayerClearance world : single = this.Get (nameof this.TileLayerClearance) world
        member this.SetTileLayerClearance (value : single) world = this.Set (nameof this.TileLayerClearance) value world
        member this.TileLayerClearance = lens (nameof this.TileLayerClearance) this this.GetTileLayerClearance this.SetTileLayerClearance
        member this.GetTileIndexOffset world : int = this.Get (nameof this.TileIndexOffset) world
        member this.SetTileIndexOffset (value : int) world = this.Set (nameof this.TileIndexOffset) value world
        member this.TileIndexOffset = lens (nameof this.TileIndexOffset) this this.GetTileIndexOffset this.SetTileIndexOffset
        member this.GetTileIndexOffsetRange world : int * int = this.Get (nameof this.TileIndexOffsetRange) world
        member this.SetTileIndexOffsetRange (value : int * int) world = this.Set (nameof this.TileIndexOffsetRange) value world
        member this.TileIndexOffsetRange = lens (nameof this.TileIndexOffsetRange) this this.GetTileIndexOffsetRange this.SetTileIndexOffsetRange
        member this.GetTileMap world : TileMap AssetTag = this.Get (nameof this.TileMap) world
        member this.SetTileMap (value : TileMap AssetTag) world = this.Set (nameof this.TileMap) value world
        member this.TileMap = lens (nameof this.TileMap) this this.GetTileMap this.SetTileMap

    /// Augments an entity with a asset-defined tile map.
    type TileMapFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.ModelDriven false
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             define Entity.TileMap Assets.Default.TileMap
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

        override this.Register (entity, world) =
            let world = entity.SetSize (entity.GetQuickSize world) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TileMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.TileMap)) entity (nameof TileMapFacet) world
            let world =
                World.sense (fun _ world ->
                    let quickSize = entity.GetQuickSize world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- quickSize
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent (nameof entity.TileMap))
                    entity
                    (nameof TileMapFacet)
                    world
            world

        override this.RegisterPhysics (entity, world) =
            match TmxMap.tryGetTileMap (entity.GetTileMap world) with
            | Some tileMap ->
                let mutable transform = entity.GetTransform world
                let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
                let tileMapPosition = perimeterUnscaled.Min.V2
                let tileMapDescriptor = TmxMap.getDescriptor tileMapPosition tileMap
                let bodyProperties =
                    TmxMap.getBodyProperties
                        transform.Enabled
                        (entity.GetFriction world)
                        (entity.GetRestitution world)
                        (entity.GetCollisionCategories world)
                        (entity.GetCollisionMask world)
                        (entity.GetBodyId world).BodyIndex
                        tileMapDescriptor
                World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world
            | None -> world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

        override this.Render (entity, world) =
            let tileMapAsset = entity.GetTileMap world
            match TmxMap.tryGetTileMap tileMapAsset with
            | Some tileMap ->
                let mutable transform = entity.GetTransform world
                let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
                let viewBounds = World.getViewBounds2dRelative world
                let tileMapMessages =
                    TmxMap.getLayeredMessages2d
                        world.GameTime
                        transform.Absolute
                        viewBounds
                        perimeterUnscaled.Min.V2
                        transform.Elevation
                        (entity.GetColor world)
                        (entity.GetEmission world)
                        (entity.GetTileLayerClearance world)
                        (entity.GetTileIndexOffset world)
                        (entity.GetTileIndexOffsetRange world)
                        tileMapAsset.PackageName
                        tileMap
                World.enqueueLayeredOperations2d tileMapMessages world
            | None -> world

        override this.GetQuickSize (entity, world) =
            match TmxMap.tryGetTileMap (entity.GetTileMap world) with
            | Some tileMap -> TmxMap.getQuickSize tileMap
            | None -> Constants.Engine.EntitySize2dDefault

[<AutoOpen>]
module TmxMapFacetModule =

    type Entity with
        member this.GetTmxMap world : TmxMap = this.Get (nameof this.TmxMap) world
        member this.SetTmxMap (value : TmxMap) world = this.Set (nameof this.TmxMap) value world
        member this.TmxMap = lens (nameof this.TmxMap) this this.GetTmxMap this.SetTmxMap

    /// Augments an entity with a user-defined tile map.
    type TmxMapFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.Presence Omnipresent
             define Entity.BodyEnabled true
             define Entity.Friction 0.0f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.ModelDriven false
             define Entity.Color Color.One
             define Entity.Emission Color.Zero
             define Entity.TileLayerClearance 2.0f
             define Entity.TileIndexOffset 0
             define Entity.TileIndexOffsetRange (0, 0)
             nonPersistent Entity.TmxMap (TmxMap.makeDefault ())
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

        override this.Register (entity, world) =
            let world = entity.SetSize (entity.GetQuickSize world) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TmxMapFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.TmxMap)) entity (nameof TmxMapFacet) world
            let world =
                World.sense (fun _ world ->
                    let quickSize = entity.GetQuickSize world
                    let mutable transform = entity.GetTransform world
                    transform.Size <- quickSize
                    let world = entity.SetTransformWithoutEvent transform world
                    (Cascade, entity.PropagatePhysics world))
                    (entity.ChangeEvent (nameof entity.TmxMap))
                    entity
                    (nameof TmxMapFacet)
                    world
            world

        override this.RegisterPhysics (entity, world) =
            let mutable transform = entity.GetTransform world
            let perimeterUnscaled = transform.PerimeterUnscaled // tmx map currently ignores rotation and scale
            let tmxMap = entity.GetTmxMap world
            let tmxMapPosition = perimeterUnscaled.Min.V2
            let tmxMapDescriptor = TmxMap.getDescriptor tmxMapPosition tmxMap
            let bodyProperties =
                TmxMap.getBodyProperties
                    transform.Enabled
                    (entity.GetFriction world)
                    (entity.GetRestitution world)
                    (entity.GetCollisionCategories world)
                    (entity.GetCollisionMask world)
                    (entity.GetBodyId world).BodyIndex
                    tmxMapDescriptor
            World.createBody (entity.GetIs2d world) (entity.GetBodyId world) bodyProperties world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody (entity.GetIs2d world) (entity.GetBodyId world) world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let perimeterUnscaled = transform.PerimeterUnscaled // tile map currently ignores rotation and scale
            let viewBounds = World.getViewBounds2dRelative world
            let tmxMap = entity.GetTmxMap world
            let tmxPackage = if tmxMap.TmxDirectory = "" then Assets.Default.PackageName else Path.GetFileName tmxMap.TmxDirectory // really folder name, but whatever...
            let tmxMapMessages =
                TmxMap.getLayeredMessages2d
                    world.GameTime
                    transform.Absolute
                    viewBounds
                    perimeterUnscaled.Min.V2
                    transform.Elevation
                    (entity.GetColor world)
                    (entity.GetEmission world)
                    (entity.GetTileLayerClearance world)
                    (entity.GetTileIndexOffset world)
                    (entity.GetTileIndexOffsetRange world)
                    tmxPackage
                    tmxMap
            World.enqueueLayeredOperations2d tmxMapMessages world

        override this.GetQuickSize (entity, world) =
            let tmxMap = entity.GetTmxMap world
            TmxMap.getQuickSize tmxMap

[<AutoOpen>]
module LayoutFacetModule =

    type Entity with
        member this.GetLayout world : Layout = this.Get (nameof this.Layout) world
        member this.SetLayout (value : Layout) world = this.Set (nameof this.Layout) value world
        member this.Layout = lens (nameof this.Layout) this this.GetLayout this.SetLayout
        member this.GetLayoutMargin world : Vector2 = this.Get (nameof this.LayoutMargin) world
        member this.SetLayoutMargin (value : Vector2) world = this.Set (nameof this.LayoutMargin) value world
        member this.LayoutMargin = lens (nameof this.LayoutMargin) this this.GetLayoutMargin this.SetLayoutMargin
        member this.GetLayoutOrder world : int = this.Get (nameof this.LayoutOrder) world
        member this.SetLayoutOrder (value : int) world = this.Set (nameof this.LayoutOrder) value world
        member this.LayoutOrder = lens (nameof this.LayoutOrder) this this.GetLayoutOrder this.SetLayoutOrder
        member this.GetDockType world : DockType = this.Get (nameof this.DockType) world
        member this.SetDockType (value : DockType) world = this.Set (nameof this.DockType) value world
        member this.DockType = lens (nameof this.DockType) this this.GetDockType this.SetDockType
        member this.GetGridPosition world : Vector2i = this.Get (nameof this.GridPosition) world
        member this.SetGridPosition (value : Vector2i) world = this.Set (nameof this.GridPosition) value world
        member this.GridPosition = lens (nameof this.GridPosition) this this.GetGridPosition this.SetGridPosition

    /// Augments an entity with the capability to perform layout transformations on its children.
    type LayoutFacet () =
        inherit Facet (false)

        static let rec flowRightward
            reentry leftX (margin : Vector2) wrapLimit (offsetX : single byref) (offsetY : single byref) (maximum : single byref) (child : Entity) world =
            let childPerimeter = child.GetPerimeter world
            let childHalfWidth = childPerimeter.Width * 0.5f
            let childHalfHeight = childPerimeter.Height * 0.5f
            let childCenter = v2 offsetX offsetY + v2 margin.X -margin.Y + v2 childHalfWidth -childHalfHeight
            let childRightX = childCenter.X + childHalfWidth + margin.X
            offsetX <- childCenter.X + childHalfWidth
            let world =
                if childRightX >= leftX + wrapLimit then
                    offsetX <- leftX
                    offsetY <- offsetY + -margin.Y + -maximum
                    maximum <- 0.0f
                    if not reentry
                    then flowRightward true leftX margin wrapLimit &offsetX &offsetY &maximum child world
                    else child.SetCenterLocal childCenter.V3 world
                else child.SetCenterLocal childCenter.V3 world
            if childPerimeter.Height > maximum then maximum <- childPerimeter.Height
            world

        static let rec flowDownward
            reentry topY (margin : Vector2) wrapLimit (offsetX : single byref) (offsetY : single byref) (maximum : single byref) (child : Entity) world =
            let childPerimeter = child.GetPerimeter world
            let childHalfWidth = childPerimeter.Width * 0.5f
            let childHalfHeight = childPerimeter.Height * 0.5f
            let childCenter = v2 offsetX offsetY + v2 margin.X -margin.Y + v2 childHalfWidth -childHalfHeight
            let childBottomY = childCenter.Y + -childHalfHeight + -margin.Y
            offsetY <- childCenter.Y + -childHalfHeight
            let world =
                if childBottomY <= topY + -wrapLimit then
                    offsetX <- offsetX + margin.X + maximum
                    offsetY <- topY
                    maximum <- 0.0f
                    if not reentry
                    then flowDownward true topY margin wrapLimit &offsetX &offsetY &maximum child world
                    else child.SetCenterLocal childCenter.V3 world
                else child.SetCenterLocal childCenter.V3 world
            if childPerimeter.Width > maximum then maximum <- childPerimeter.Width
            world

        static let flowLayout (perimeter : Box2) margin flowDirection flowLimit children world =
            let leftX = perimeter.Width * -0.5f
            let topY = perimeter.Height * 0.5f
            let mutable offsetX = leftX
            let mutable offsetY = topY
            let mutable maximum = 0.0f
            match flowDirection with
            | FlowRightward ->
                let wrapLimit =
                    match flowLimit with
                    | FlowParent -> perimeter.Width
                    | FlowUnlimited -> Single.MaxValue
                    | FlowTo flowLimit -> flowLimit
                Array.fold (fun world child ->
                    flowRightward false leftX margin wrapLimit &offsetX &offsetY &maximum child world)
                    world children
            | FlowDownward ->
                let wrapLimit =
                    match flowLimit with
                    | FlowParent -> perimeter.Height
                    | FlowUnlimited -> Single.MaxValue
                    | FlowTo flowLimit -> flowLimit
                Array.fold (fun world child ->
                    flowDownward false topY margin wrapLimit &offsetX &offsetY &maximum child world)
                    world children
            | FlowLeftward -> world
            | FlowUpward -> world

        static let dockLayout (perimeter : Box2) margin (margins : Vector4) children world =
            let perimeterWidthHalf = perimeter.Width * 0.5f
            let perimeterHeightHalf = perimeter.Height * 0.5f
            Array.fold (fun world (child : Entity) ->
                if child.Has<LayoutFacet> world then
                    match child.GetDockType world with
                    | DockCenter ->
                        let size =
                            v2
                                (perimeter.Width - margins.X - margins.Z)
                                (perimeter.Height - margins.Y - margins.W) -
                            margin
                        let position =
                            v2
                                ((margins.X - margins.Z) * 0.5f)
                                ((margins.Y - margins.W) * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                    | DockTop ->
                        let size = v2 perimeter.Width margins.W - margin
                        let position = v2 0.0f (perimeterHeightHalf - margins.Z * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                    | DockRight ->
                        let size = v2 margins.Z (perimeter.Height - margins.Y - margins.W) - margin
                        let position = v2 (perimeterWidthHalf - margins.Z * 0.5f) ((margins.Y - margins.W) * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                    | DockBottom ->
                        let size = v2 perimeter.Width margins.Y - margin
                        let position = v2 0.0f (-perimeterHeightHalf + margins.Y * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                    | DockLeft ->
                        let size = v2 margins.X (perimeter.Height - margins.Y - margins.W) - margin
                        let position = v2 (-perimeterWidthHalf + margins.X * 0.5f) ((margins.Y - margins.W) * 0.5f)
                        let world = child.SetPositionLocal position.V3 world
                        child.SetSize size.V3 world
                else world)
                world children

        static let gridLayout (perimeter : Box2) margin (dims : Vector2i) flowDirectionOpt resizeChildren children world =
            let perimeterWidthHalf = perimeter.Width * 0.5f
            let perimeterHeightHalf = perimeter.Height * 0.5f
            let cellSize = v2 (perimeter.Width / single dims.X) (perimeter.Height / single dims.Y)
            let cellWidthHalf = cellSize.X * 0.5f
            let cellHeightHalf = cellSize.Y * 0.5f
            let childSize = cellSize - margin
            Array.foldi (fun n world (child : Entity) ->
                let (i, j) =
                    match flowDirectionOpt with
                    | Some flowDirection ->
                        match flowDirection with
                        | FlowRightward -> (n % dims.Y, n / dims.Y)
                        | FlowDownward -> (n / dims.Y, n % dims.Y)
                        | FlowLeftward -> (dec dims.Y - n % dims.Y, dec dims.Y - n / dims.Y)
                        | FlowUpward -> (dec dims.Y - n / dims.Y, dec dims.Y - n % dims.Y)
                    | None ->
                        if child.Has<LayoutFacet> world then
                            let gridPosition = child.GetGridPosition world
                            (gridPosition.X, gridPosition.Y)
                        else (0, 0)
                let childPosition =
                    v2
                        (-perimeterWidthHalf + single i * cellSize.X + cellWidthHalf)
                        (perimeterHeightHalf - single j * cellSize.Y - cellHeightHalf)
                let world = child.SetPositionLocal childPosition.V3 world
                if resizeChildren
                then child.SetSize childSize.V3 world
                else world)
                world children

        static let performLayout (entity : Entity) world =
            if entity.GetCentered world then // NOTE: layouts only supported for centered entities.
                match entity.GetLayout world with
                | Manual -> world // OPTIMIZATION: early exit.
                | layout ->
                    let children =
                        World.getEntityMounters entity world |>
                        Array.ofSeq |>
                        Array.map (fun child ->
                            let layoutOrder =
                                if child.Has<LayoutFacet> world
                                then child.GetLayoutOrder world
                                else 0
                            let order = child.GetOrder world
                            (layoutOrder, order, child)) |>
                        Array.sortBy ab_ |>
                        Array.map __c
                    let perimeter = (entity.GetPerimeter world).Box2
                    let margin = entity.GetLayoutMargin world
                    let world =
                        match layout with
                        | Flow (flowDirection, flowLimit) ->
                            flowLayout perimeter margin flowDirection flowLimit children world
                        | Dock (margins, percentageBased, resizeChildren) ->
                            ignore (percentageBased, resizeChildren) // TODO: implement using these values.
                            dockLayout perimeter margin margins children world
                        | Grid (dims, flowDirectionOpt, resizeChildren) ->
                            gridLayout perimeter margin dims flowDirectionOpt resizeChildren children world
                        | Manual -> world
                    world
            else world

        static let handleLayout evt world =
            let entity = evt.Subscriber : Entity
            (Cascade, performLayout entity world)

        static let handleMount evt world =
            let entity = evt.Subscriber : Entity
            let mounter = evt.Data.Mounter
            let (orderChangeUnsub, world) = World.sensePlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.Order.ChangeEvent --> mounter) entity (nameof LayoutFacet) world
            let (layoutOrderChangeUnsub, world) = World.sensePlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.LayoutOrder.ChangeEvent --> mounter) entity (nameof LayoutFacet) world
            let (dockTypeChangeUnsub, world) = World.sensePlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.DockType.ChangeEvent --> mounter) entity (nameof LayoutFacet) world
            let (gridPositionChangeUnsub, world) = World.sensePlus (fun _ world -> (Cascade, performLayout entity world)) (Entity.GridPosition.ChangeEvent --> mounter) entity (nameof LayoutFacet) world
            let world =
                World.sense (fun evt world ->
                    let world =
                        if evt.Data.Mounter = mounter then
                            let world = world |> orderChangeUnsub |> layoutOrderChangeUnsub |> dockTypeChangeUnsub |> gridPositionChangeUnsub
                            performLayout entity world
                        else world
                    (Cascade, world))
                    entity.UnmountEvent
                    entity
                    (nameof LayoutFacet)
                    world
            let world = performLayout entity world
            (Cascade, world)

        static member Properties =
            [define Entity.Layout Manual
             define Entity.LayoutMargin v2Zero
             define Entity.LayoutOrder 0
             define Entity.DockType DockCenter
             define Entity.GridPosition v2iZero]

        override this.Register (entity, world) =
            let world = performLayout entity world
            let world = World.sense handleMount entity.MountEvent entity (nameof LayoutFacet) world
            let world = World.sense handleLayout entity.Transform.ChangeEvent entity (nameof LayoutFacet) world
            let world = World.sense handleLayout entity.Layout.ChangeEvent entity (nameof LayoutFacet) world
            let world = World.sense handleLayout entity.LayoutMargin.ChangeEvent entity (nameof LayoutFacet) world
            world

[<AutoOpen>]
module SkyBoxFacetModule =

    type Entity with
        member this.GetAmbientColor world : Color = this.Get (nameof this.AmbientColor) world
        member this.SetAmbientColor (value : Color) world = this.Set (nameof this.AmbientColor) value world
        member this.AmbientColor = lens (nameof this.AmbientColor) this this.GetAmbientColor this.SetAmbientColor
        member this.GetAmbientBrightness world : single = this.Get (nameof this.AmbientBrightness) world
        member this.SetAmbientBrightness (value : single) world = this.Set (nameof this.AmbientBrightness) value world
        member this.AmbientBrightness = lens (nameof this.AmbientBrightness) this this.GetAmbientBrightness this.SetAmbientBrightness
        member this.GetBrightness world : single = this.Get (nameof this.Brightness) world
        member this.SetBrightness (value : single) world = this.Set (nameof this.Brightness) value world
        member this.Brightness = lens (nameof this.Brightness) this this.GetBrightness this.SetBrightness
        member this.GetCubeMap world : CubeMap AssetTag = this.Get (nameof this.CubeMap) world
        member this.SetCubeMap (value : CubeMap AssetTag) world = this.Set (nameof this.CubeMap) value world
        member this.CubeMap = lens (nameof this.CubeMap) this this.GetCubeMap this.SetCubeMap

    /// Augments an entity with sky box.
    type SkyBoxFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Absolute true
             define Entity.Presence Omnipresent
             define Entity.AmbientColor Color.White
             define Entity.AmbientBrightness 1.0f
             define Entity.Color Color.White
             define Entity.Brightness 1.0f
             define Entity.CubeMap Assets.Default.SkyBoxMap]

        override this.Render (entity, world) =
            World.enqueueRenderMessage3d
                (RenderSkyBox
                    { AmbientColor = entity.GetAmbientColor world
                      AmbientBrightness = entity.GetAmbientBrightness world
                      CubeMapColor = entity.GetColor world
                      CubeMapBrightness = entity.GetBrightness world
                      CubeMap = entity.GetCubeMap world })
                world

[<AutoOpen>]
module LightProbeFacet3dModule =

    type Entity with
        member this.GetProbeBounds world : Box3 = this.Get (nameof this.ProbeBounds) world
        member this.SetProbeBounds (value : Box3) world = this.Set (nameof this.ProbeBounds) value world
        member this.ProbeBounds = lens (nameof this.ProbeBounds) this this.GetProbeBounds this.SetProbeBounds
        member this.GetProbeStale world : bool = this.Get (nameof this.ProbeStale) world
        member this.SetProbeStale (value : bool) world = this.Set (nameof this.ProbeStale) value world
        member this.ProbeStale = lens (nameof this.ProbeStale) this this.GetProbeStale this.SetProbeStale

    /// Augments an entity with a 3d light probe.
    type LightProbeFacet3d () =
        inherit Facet (false)

        static let handleProbeStaleChange (evt : Event<ChangeData, Entity>) world =
            let world =
                if evt.Data.Value :?> bool
                then World.requestUnculledRender world
                else world
            (Cascade, world)

        static member Properties =
            [define Entity.LightProbe true
             define Entity.Presence Omnipresent
             define Entity.ProbeBounds (box3 (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f) (v3Dup Constants.Render.LightProbeSizeDefault))
             define Entity.ProbeStale false]

        override this.Register (entity, world) =
            let world = World.sense handleProbeStaleChange (entity.GetChangeEvent (nameof entity.ProbeStale)) entity (nameof LightProbeFacet3d) world
            entity.SetProbeStale true world
            
        override this.Render (entity, world) =
            let id = entity.GetId world
            let enabled = entity.GetEnabled world
            let position = entity.GetPosition world
            let bounds = entity.GetProbeBounds world
            let stale = entity.GetProbeStale world
            let world = if stale then entity.SetProbeStale false world else world
            World.enqueueRenderMessage3d (RenderLightProbe3d { LightProbeId = id; Enabled = enabled; Origin = position; Bounds = bounds; Stale = stale }) world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

        override this.TryGetHighlightBounds (entity, world) =
            Some (entity.GetBounds world)

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module LightFacet3dModule =

    type Entity with
        member this.GetAttenuationLinear world : single = this.Get (nameof this.AttenuationLinear) world
        member this.SetAttenuationLinear (value : single) world = this.Set (nameof this.AttenuationLinear) value world
        member this.AttenuationLinear = lens (nameof this.AttenuationLinear) this this.GetAttenuationLinear this.SetAttenuationLinear
        member this.GetAttenuationQuadratic world : single = this.Get (nameof this.AttenuationQuadratic) world
        member this.SetAttenuationQuadratic (value : single) world = this.Set (nameof this.AttenuationQuadratic) value world
        member this.AttenuationQuadratic = lens (nameof this.AttenuationQuadratic) this this.GetAttenuationQuadratic this.SetAttenuationQuadratic
        member this.GetCutoff world : single = this.Get (nameof this.Cutoff) world
        member this.SetCutoff (value : single) world = this.Set (nameof this.Cutoff) value world
        member this.Cutoff = lens (nameof this.Cutoff) this this.GetCutoff this.SetCutoff
        member this.GetLightType world : LightType = this.Get (nameof this.LightType) world
        member this.SetLightType (value : LightType) world = this.Set (nameof this.LightType) value world
        member this.LightType = lens (nameof this.LightType) this this.GetLightType this.SetLightType

    /// Augments an entity with a 3d light.
    type LightFacet3d () =
        inherit Facet (false)

        static member Properties =
            [define Entity.Light true
             define Entity.Color Color.White
             define Entity.Brightness Constants.Render.BrightnessDefault
             define Entity.AttenuationLinear Constants.Render.AttenuationLinearDefault
             define Entity.AttenuationQuadratic Constants.Render.AttenuationQuadraticDefault
             define Entity.Cutoff Constants.Render.CutoffDefault
             define Entity.LightType PointLight]

        override this.Render (entity, world) =
            if entity.GetEnabled world then
                let position = entity.GetPosition world
                let rotation = entity.GetRotation world
                let color = entity.GetColor world
                let brightness = entity.GetBrightness world
                let attenuationLinear = entity.GetAttenuationLinear world
                let attenuationQuadratic = entity.GetAttenuationQuadratic world
                let cutoff = entity.GetCutoff world
                let lightType = entity.GetLightType world
                World.enqueueRenderMessage3d
                    (RenderLight3d
                        { Origin = position
                          Direction = Vector3.Transform (v3Up, rotation)
                          Color = color
                          Brightness = brightness
                          AttenuationLinear = attenuationLinear
                          AttenuationQuadratic = attenuationQuadratic
                          Cutoff = cutoff
                          LightType = lightType })
                    world
            else world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

        override this.TryGetHighlightBounds (entity, world) =
            Some (entity.GetBounds world)

        override this.GetQuickSize (_, _) =
            v3Dup 0.5f

[<AutoOpen>]
module StaticBillboardFacetModule =

    /// Determines the means by which an entity's surfaces are rendered.
    type [<StructuralEquality; StructuralComparison>] RenderStyle =
        | Deferred
        | Forward of Subsort : single * Sort : single

    type Entity with
        // OPTIMIZATION: override allows surface properties to be fetched with a single look-up.
        member this.GetMaterialProperties world : MaterialProperties = this.Get (nameof this.MaterialProperties) world
        member this.SetMaterialProperties (value : MaterialProperties) world = this.Set (nameof this.MaterialProperties) value world
        member this.MaterialProperties = lens (nameof this.MaterialProperties) this this.GetMaterialProperties this.SetMaterialProperties
        member this.GetAlbedoImage world : Image AssetTag = this.Get (nameof this.AlbedoImage) world
        member this.SetAlbedoImage (value : Image AssetTag) world = this.Set (nameof this.AlbedoImage) value world
        member this.AlbedoImage = lens (nameof this.AlbedoImage) this this.GetAlbedoImage this.SetAlbedoImage
        member this.GetRoughnessImage world : Image AssetTag = this.Get (nameof this.RoughnessImage) world
        member this.SetRoughnessImage (value : Image AssetTag) world = this.Set (nameof this.RoughnessImage) value world
        member this.RoughnessImage = lens (nameof this.RoughnessImage) this this.GetRoughnessImage this.SetRoughnessImage
        member this.GetMetallicImage world : Image AssetTag = this.Get (nameof this.MetallicImage) world
        member this.SetMetallicImage (value : Image AssetTag) world = this.Set (nameof this.MetallicImage) value world
        member this.MetallicImage = lens (nameof this.MetallicImage) this this.GetMetallicImage this.SetMetallicImage
        member this.GetAmbientOcclusionImage world : Image AssetTag = this.Get (nameof this.AmbientOcclusionImage) world
        member this.SetAmbientOcclusionImage (value : Image AssetTag) world = this.Set (nameof this.AmbientOcclusionImage) value world
        member this.AmbientOcclusionImage = lens (nameof this.AmbientOcclusionImage) this this.GetAmbientOcclusionImage this.SetAmbientOcclusionImage
        member this.GetEmissionImage world : Image AssetTag = this.Get (nameof this.EmissionImage) world
        member this.SetEmissionImage (value : Image AssetTag) world = this.Set (nameof this.EmissionImage) value world
        member this.EmissionImage = lens (nameof this.EmissionImage) this this.GetEmissionImage this.SetEmissionImage
        member this.GetNormalImage world : Image AssetTag = this.Get (nameof this.NormalImage) world
        member this.SetNormalImage (value : Image AssetTag) world = this.Set (nameof this.NormalImage) value world
        member this.NormalImage = lens (nameof this.NormalImage) this this.GetNormalImage this.SetNormalImage
        member this.GetHeightImage world : Image AssetTag = this.Get (nameof this.HeightImage) world
        member this.SetHeightImage (value : Image AssetTag) world = this.Set (nameof this.HeightImage) value world
        member this.HeightImage = lens (nameof this.HeightImage) this this.GetHeightImage this.SetHeightImage
        member this.GetTextureMinFilterOpt world : OpenGL.TextureMinFilter option = this.Get (nameof this.TextureMinFilterOpt) world
        member this.SetTextureMinFilterOpt (value : OpenGL.TextureMinFilter option) world = this.Set (nameof this.TextureMinFilterOpt) value world
        member this.TextureMinFilterOpt = lens (nameof this.TextureMinFilterOpt) this this.GetTextureMinFilterOpt this.SetTextureMinFilterOpt
        member this.GetTextureMagFilterOpt world : OpenGL.TextureMagFilter option = this.Get (nameof this.TextureMagFilterOpt) world
        member this.SetTextureMagFilterOpt (value : OpenGL.TextureMagFilter option) world = this.Set (nameof this.TextureMagFilterOpt) value world
        member this.TextureMagFilterOpt = lens (nameof this.TextureMagFilterOpt) this this.GetTextureMagFilterOpt this.SetTextureMagFilterOpt
        member this.GetRenderStyle world : RenderStyle = this.Get (nameof this.RenderStyle) world
        member this.SetRenderStyle (value : RenderStyle) world = this.Set (nameof this.RenderStyle) value world
        member this.RenderStyle = lens (nameof this.RenderStyle) this this.GetRenderStyle this.SetRenderStyle

    /// Augments an entity with a static billboard.
    type StaticBillboardFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.AlbedoImage Assets.Default.MaterialAlbedo
             define Entity.RoughnessImage Assets.Default.MaterialRoughness
             define Entity.MetallicImage Assets.Default.MaterialMetallic
             define Entity.AmbientOcclusionImage Assets.Default.MaterialAmbientOcclusion
             define Entity.EmissionImage Assets.Default.MaterialEmission
             define Entity.NormalImage Assets.Default.MaterialNormal
             define Entity.HeightImage Assets.Default.MaterialHeight
             define Entity.TextureMinFilterOpt None
             define Entity.TextureMagFilterOpt None
             define Entity.RenderStyle Deferred]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrixOffset = transform.AffineMatrixOffset
            let insetOpt = entity.GetInsetOpt world
            let properties = entity.GetMaterialProperties world
            let albedoImage = entity.GetAlbedoImage world
            let roughnessImage = entity.GetRoughnessImage world
            let metallicImage = entity.GetMetallicImage world
            let ambientOcclusionImage = entity.GetAmbientOcclusionImage world
            let emissionImage = entity.GetEmissionImage world
            let normalImage = entity.GetNormalImage world
            let heightImage = entity.GetHeightImage world
            let minFilterOpt = entity.GetTextureMinFilterOpt world
            let magFilterOpt = entity.GetTextureMagFilterOpt world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            World.enqueueRenderMessage3d
                (RenderBillboard
                    { Absolute = absolute; ModelMatrix = affineMatrixOffset; InsetOpt = insetOpt; MaterialProperties = properties
                      AlbedoImage = albedoImage; RoughnessImage = roughnessImage; MetallicImage = metallicImage; AmbientOcclusionImage = ambientOcclusionImage; EmissionImage = emissionImage; NormalImage = normalImage; HeightImage = heightImage
                      MinFilterOpt = minFilterOpt; MagFilterOpt = magFilterOpt; RenderType = renderType })
                world

        override this.RayCast (ray, entity, world) =
            // TODO: intersect against oriented quad rather than box.
            match this.TryGetHighlightBounds (entity, world) with
            | Some bounds ->
                let intersectionOpt = ray.Intersects bounds
                if intersectionOpt.HasValue then [|intersectionOpt.Value|]
                else [||]
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            let bounds = entity.GetBounds world
            Some bounds

[<AutoOpen>]
module BasicStaticBillboardEmitterFacetModule =

    type Entity with

        member this.GetEmitterMaterialProperties world : MaterialProperties = this.Get (nameof this.EmitterMaterialProperties) world
        member this.SetEmitterMaterialProperties (value : MaterialProperties) world = this.Set (nameof this.EmitterMaterialProperties) value world
        member this.EmitterMaterialProperties = lens (nameof this.EmitterMaterialProperties) this this.GetEmitterMaterialProperties this.SetEmitterMaterialProperties
        member this.GetEmitterAlbedoImage world : Image AssetTag = this.Get (nameof this.EmitterAlbedoImage) world
        member this.SetEmitterAlbedoImage (value : Image AssetTag) world = this.Set (nameof this.EmitterAlbedoImage) value world
        member this.EmitterAlbedoImage = lens (nameof this.EmitterAlbedoImage) this this.GetEmitterAlbedoImage this.SetEmitterAlbedoImage
        member this.GetEmitterRoughnessImage world : Image AssetTag = this.Get (nameof this.EmitterRoughnessImage) world
        member this.SetEmitterRoughnessImage (value : Image AssetTag) world = this.Set (nameof this.EmitterRoughnessImage) value world
        member this.EmitterRoughnessImage = lens (nameof this.EmitterRoughnessImage) this this.GetEmitterRoughnessImage this.SetEmitterRoughnessImage
        member this.GetEmitterMetallicImage world : Image AssetTag = this.Get (nameof this.EmitterMetallicImage) world
        member this.SetEmitterMetallicImage (value : Image AssetTag) world = this.Set (nameof this.EmitterMetallicImage) value world
        member this.EmitterMetallicImage = lens (nameof this.EmitterMetallicImage) this this.GetEmitterMetallicImage this.SetEmitterMetallicImage
        member this.GetEmitterAmbientOcclusionImage world : Image AssetTag = this.Get (nameof this.EmitterAmbientOcclusionImage) world
        member this.SetEmitterAmbientOcclusionImage (value : Image AssetTag) world = this.Set (nameof this.EmitterAmbientOcclusionImage) value world
        member this.EmitterAmbientOcclusionImage = lens (nameof this.EmitterAmbientOcclusionImage) this this.GetEmitterAmbientOcclusionImage this.SetEmitterAmbientOcclusionImage
        member this.GetEmitterEmissionImage world : Image AssetTag = this.Get (nameof this.EmitterEmissionImage) world
        member this.SetEmitterEmissionImage (value : Image AssetTag) world = this.Set (nameof this.EmitterEmissionImage) value world
        member this.EmitterEmissionImage = lens (nameof this.EmitterEmissionImage) this this.GetEmitterEmissionImage this.SetEmitterEmissionImage
        member this.GetEmitterNormalImage world : Image AssetTag = this.Get (nameof this.EmitterNormalImage) world
        member this.SetEmitterNormalImage (value : Image AssetTag) world = this.Set (nameof this.EmitterNormalImage) value world
        member this.EmitterNormalImage = lens (nameof this.EmitterNormalImage) this this.GetEmitterNormalImage this.SetEmitterNormalImage
        member this.GetEmitterHeightImage world : Image AssetTag = this.Get (nameof this.EmitterHeightImage) world
        member this.SetEmitterHeightImage (value : Image AssetTag) world = this.Set (nameof this.EmitterHeightImage) value world
        member this.EmitterHeightImage = lens (nameof this.EmitterHeightImage) this this.GetEmitterHeightImage this.SetEmitterHeightImage
        member this.GetEmitterMinFilterOpt world : OpenGL.TextureMinFilter option = this.Get (nameof this.EmitterMinFilterOpt) world
        member this.SetEmitterMinFilterOpt (value : OpenGL.TextureMinFilter option) world = this.Set (nameof this.EmitterMinFilterOpt) value world
        member this.EmitterMinFilterOpt = lens (nameof this.EmitterMinFilterOpt) this this.GetEmitterMinFilterOpt this.SetEmitterMinFilterOpt
        member this.GetEmitterMagFilterOpt world : OpenGL.TextureMagFilter option = this.Get (nameof this.EmitterMagFilterOpt) world
        member this.SetEmitterMagFilterOpt (value : OpenGL.TextureMagFilter option) world = this.Set (nameof this.EmitterMagFilterOpt) value world
        member this.EmitterMagFilterOpt = lens (nameof this.EmitterMagFilterOpt) this this.GetEmitterMagFilterOpt this.SetEmitterMagFilterOpt
        member this.GetEmitterRenderType world : RenderType = this.Get (nameof this.EmitterRenderType) world
        member this.SetEmitterRenderType (value : RenderType) world = this.Set (nameof this.EmitterRenderType) value world
        member this.EmitterRenderType = lens (nameof this.EmitterRenderType) this this.GetEmitterRenderType this.SetEmitterRenderType

    /// Augments an entity with basic static billboard emitter.
    type BasicStaticBillboardEmitterFacet () =
        inherit Facet (false)

        static let tryMakeEmitter (entity : Entity) (world : World) =
            World.tryMakeEmitter
                world.GameTime
                (entity.GetEmitterLifeTimeOpt world)
                (entity.GetParticleLifeTimeMaxOpt world)
                (entity.GetParticleRate world)
                (entity.GetParticleMax world)
                (entity.GetEmitterStyle world)
                world |>
            Option.map cast<Particles.BasicStaticBillboardEmitter>

        static let makeEmitter entity world =
            match tryMakeEmitter entity world with
            | Some emitter ->
                let mutable transform = entity.GetTransform world
                { emitter with
                    Body =
                        { Position = transform.Position
                          Scale = transform.Scale
                          Angles = transform.Angles
                          LinearVelocity = v3Zero
                          AngularVelocity = v3Zero
                          Restitution = Constants.Particles.RestitutionDefault }
                    Absolute = transform.Absolute
                    AlbedoImage = entity.GetEmitterAlbedoImage world
                    RoughnessImage = entity.GetEmitterRoughnessImage world
                    MetallicImage = entity.GetEmitterMetallicImage world
                    AmbientOcclusionImage = entity.GetEmitterAmbientOcclusionImage world
                    EmissionImage = entity.GetEmitterEmissionImage world
                    NormalImage = entity.GetEmitterNormalImage world
                    HeightImage = entity.GetEmitterHeightImage world
                    ParticleSeed = entity.GetBasicParticleSeed world
                    Constraint = entity.GetEmitterConstraint world }
            | None ->
                Particles.BasicStaticBillboardEmitter.makeEmpty
                    world.GameTime
                    (entity.GetEmitterLifeTimeOpt world)
                    (entity.GetParticleLifeTimeMaxOpt world)
                    (entity.GetParticleRate world)
                    (entity.GetParticleMax world)

        static let updateParticleSystem updater (entity : Entity) world =
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = updater particleSystem
            let world = entity.SetParticleSystem particleSystem world
            world

        static let updateEmitter updater (entity : Entity) world =
            updateParticleSystem (fun particleSystem ->
                match Map.tryFind typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicStaticBillboardEmitter as emitter) ->
                    let emitter = updater emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem)
                entity world

        static let rec processOutput output entity world =
            match output with
            | Particles.OutputSound (volume, sound) -> World.enqueueAudioMessage (PlaySoundMessage { Volume = volume; Sound = sound }) world
            | Particles.OutputEmitter (name, emitter) -> updateParticleSystem (fun ps -> { ps with Emitters = Map.add name emitter ps.Emitters }) entity world
            | Particles.Outputs outputs -> SArray.fold (fun world output -> processOutput output entity world) world outputs

        static let handleEmitterMaterialPropertiesChange evt world =
            let emitterMaterialProperties = evt.Data.Value :?> MaterialProperties
            let world = updateEmitter (fun emitter -> if emitter.MaterialProperties <> emitterMaterialProperties then { emitter with MaterialProperties = emitterMaterialProperties } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterAlbedoImageChange evt world =
            let emitterAlbedoImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.AlbedoImage emitterAlbedoImage then { emitter with AlbedoImage = emitterAlbedoImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterRoughnessImageChange evt world =
            let emitterRoughnessImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.RoughnessImage emitterRoughnessImage then { emitter with RoughnessImage = emitterRoughnessImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterMetallicImageChange evt world =
            let emitterMetallicImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.MetallicImage emitterMetallicImage then { emitter with MetallicImage = emitterMetallicImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterAmbientOcclusionImageChange evt world =
            let emitterAmbientOcclusionImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.AmbientOcclusionImage emitterAmbientOcclusionImage then { emitter with AmbientOcclusionImage = emitterAmbientOcclusionImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterEmissionImageChange evt world =
            let emitterEmissionImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.EmissionImage emitterEmissionImage then { emitter with EmissionImage = emitterEmissionImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterNormalImageChange evt world =
            let emitterNormalImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.NormalImage emitterNormalImage then { emitter with NormalImage = emitterNormalImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterHeightImageChange evt world =
            let emitterHeightImage = evt.Data.Value :?> Image AssetTag
            let world = updateEmitter (fun emitter -> if assetNeq emitter.HeightImage emitterHeightImage then { emitter with HeightImage = emitterHeightImage } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterMinFilterOptChange evt world =
            let emitterMinFilterOpt = evt.Data.Value :?> OpenGL.TextureMinFilter option
            let world = updateEmitter (fun emitter -> if emitter.MinFilterOpt <> emitterMinFilterOpt then { emitter with MinFilterOpt = emitterMinFilterOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterMagFilterOptChange evt world =
            let emitterMagFilterOpt = evt.Data.Value :?> OpenGL.TextureMagFilter option
            let world = updateEmitter (fun emitter -> if emitter.MagFilterOpt <> emitterMagFilterOpt then { emitter with MagFilterOpt = emitterMagFilterOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterRenderTypeChange evt world =
            let emitterRenderType = evt.Data.Value :?> RenderType
            let world = updateEmitter (fun emitter -> if emitter.RenderType <> emitterRenderType then { emitter with RenderType = emitterRenderType } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterLifeTimeOptChange evt world =
            let emitterLifeTimeOpt = evt.Data.Value :?> GameTime
            let world = updateEmitter (fun emitter -> if emitter.Life.LifeTimeOpt <> emitterLifeTimeOpt then { emitter with Life = { emitter.Life with LifeTimeOpt = emitterLifeTimeOpt }} else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleLifeTimeMaxOptChange evt world =
            let particleLifeTimeMaxOpt = evt.Data.Value :?> GameTime
            let world = updateEmitter (fun emitter -> if emitter.ParticleLifeTimeMaxOpt <> particleLifeTimeMaxOpt then { emitter with ParticleLifeTimeMaxOpt = particleLifeTimeMaxOpt } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleRateChange evt world =
            let particleRate = evt.Data.Value :?> single
            let world = updateEmitter (fun emitter -> if emitter.ParticleRate <> particleRate then { emitter with ParticleRate = particleRate } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleParticleMaxChange evt world =
            let particleMax = evt.Data.Value :?> int
            let world = updateEmitter (fun emitter -> if emitter.ParticleRing.Length <> particleMax then Particles.BasicStaticBillboardEmitter.resize particleMax emitter else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleBasicParticleSeedChange evt world =
            let particleSeed = evt.Data.Value :?> Particles.BasicParticle
            let world = updateEmitter (fun emitter -> if emitter.ParticleSeed <> particleSeed then { emitter with ParticleSeed = particleSeed } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterConstraintChange evt world =
            let emitterConstraint = evt.Data.Value :?> Particles.Constraint
            let world = updateEmitter (fun emitter -> if emitter.Constraint <> emitterConstraint then { emitter with Constraint = emitterConstraint } else emitter) evt.Subscriber world
            (Cascade, world)

        static let handleEmitterStyleChange evt world =
            let entity = evt.Subscriber
            let emitter = makeEmitter entity world
            let world = updateEmitter (constant emitter) entity world
            (Cascade, world)

        static let handlePositionChange evt world =
            let entity = evt.Subscriber : Entity
            let particleSystem = entity.GetParticleSystem world
            let particleSystem =
                match Map.tryFind typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicStaticBillboardEmitter as emitter) ->
                    let position = entity.GetPosition world
                    let emitter =
                        if v3Neq emitter.Body.Position position
                        then { emitter with Body = { emitter.Body with Position = position }}
                        else emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

        static let handleRotationChange evt world =
            let entity = evt.Subscriber : Entity
            let particleSystem = entity.GetParticleSystem world
            let particleSystem =
                match Map.tryFind typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters with
                | Some (:? Particles.BasicStaticBillboardEmitter as emitter) ->
                    let angles = entity.GetAngles world
                    let emitter =
                        if v3Neq emitter.Body.Angles angles
                        then { emitter with Body = { emitter.Body with Angles = angles }}
                        else emitter
                    { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
                | _ -> particleSystem
            let world = entity.SetParticleSystem particleSystem world
            (Cascade, world)

        static member Properties =
            [define Entity.SelfDestruct false
             define Entity.EmitterMaterialProperties MaterialProperties.defaultProperties
             define Entity.EmitterAlbedoImage Assets.Default.MaterialAlbedo
             define Entity.EmitterRoughnessImage Assets.Default.MaterialRoughness
             define Entity.EmitterMetallicImage Assets.Default.MaterialMetallic
             define Entity.EmitterAmbientOcclusionImage Assets.Default.MaterialAmbientOcclusion
             define Entity.EmitterEmissionImage Assets.Default.MaterialEmission
             define Entity.EmitterNormalImage Assets.Default.MaterialNormal
             define Entity.EmitterHeightImage Assets.Default.MaterialHeight
             define Entity.EmitterMinFilterOpt None
             define Entity.EmitterMagFilterOpt None
             define Entity.EmitterLifeTimeOpt GameTime.zero
             define Entity.ParticleLifeTimeMaxOpt (GameTime.ofSeconds 1.0f)
             define Entity.ParticleRate (match Constants.GameTime.DesiredFrameRate with StaticFrameRate _ -> 1.0f | DynamicFrameRate _ -> 60.0f)
             define Entity.ParticleMax 60
             define Entity.BasicParticleSeed { Life = Particles.Life.make GameTime.zero (GameTime.ofSeconds 1.0f); Body = Particles.Body.defaultBody; Size = Constants.Engine.ParticleSize3dDefault; Offset = v3Zero; Inset = box2Zero; Color = Color.One; Emission = Color.Zero; Flip = FlipNone }
             define Entity.EmitterConstraint Particles.Constraint.empty
             define Entity.EmitterStyle "BasicStaticBillboardEmitter"
             nonPersistent Entity.ParticleSystem Particles.ParticleSystem.empty]

        override this.Register (entity, world) =
            let emitter = makeEmitter entity world
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.add typeof<Particles.BasicStaticBillboardEmitter>.Name (emitter :> Particles.Emitter) particleSystem.Emitters }
            let world = entity.SetParticleSystem particleSystem world
            let world = World.sense handlePositionChange (entity.GetChangeEvent (nameof entity.Position)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleRotationChange (entity.GetChangeEvent (nameof entity.Rotation)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterMaterialPropertiesChange (entity.GetChangeEvent (nameof entity.EmitterMaterialProperties)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterAlbedoImageChange (entity.GetChangeEvent (nameof entity.EmitterAlbedoImage)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterRoughnessImageChange (entity.GetChangeEvent (nameof entity.EmitterRoughnessImage)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterMetallicImageChange (entity.GetChangeEvent (nameof entity.EmitterMetallicImage)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterAmbientOcclusionImageChange (entity.GetChangeEvent (nameof entity.EmitterAmbientOcclusionImage)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterEmissionImageChange (entity.GetChangeEvent (nameof entity.EmitterEmissionImage)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterNormalImageChange (entity.GetChangeEvent (nameof entity.EmitterNormalImage)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterHeightImageChange (entity.GetChangeEvent (nameof entity.EmitterHeightImage)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterMinFilterOptChange (entity.GetChangeEvent (nameof entity.EmitterMinFilterOpt)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterMagFilterOptChange (entity.GetChangeEvent (nameof entity.EmitterMagFilterOpt)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterRenderTypeChange (entity.GetChangeEvent (nameof entity.EmitterRenderType)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterLifeTimeOptChange (entity.GetChangeEvent (nameof entity.EmitterLifeTimeOpt)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleParticleLifeTimeMaxOptChange (entity.GetChangeEvent (nameof entity.ParticleLifeTimeMaxOpt)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleParticleRateChange (entity.GetChangeEvent (nameof entity.ParticleRate)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleParticleMaxChange (entity.GetChangeEvent (nameof entity.ParticleMax)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleBasicParticleSeedChange (entity.GetChangeEvent (nameof entity.BasicParticleSeed)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterConstraintChange (entity.GetChangeEvent (nameof entity.EmitterConstraint)) entity (nameof BasicStaticBillboardEmitterFacet) world
            let world = World.sense handleEmitterStyleChange (entity.GetChangeEvent (nameof entity.EmitterStyle)) entity (nameof BasicStaticBillboardEmitterFacet) world
            world

        override this.Unregister (entity, world) =
            let particleSystem = entity.GetParticleSystem world
            let particleSystem = { particleSystem with Emitters = Map.remove typeof<Particles.BasicStaticBillboardEmitter>.Name particleSystem.Emitters }
            entity.SetParticleSystem particleSystem world

        override this.Update (entity, world) =
            if entity.GetEnabled world then
                let delta = world.GameDelta
                let time = world.GameTime
                let particleSystem = entity.GetParticleSystem world
                let (particleSystem, output) = Particles.ParticleSystem.run delta time particleSystem
                let world = entity.SetParticleSystem particleSystem world
                processOutput output entity world
            else world

        override this.Render (entity, world) =
            let time = world.GameTime
            let particleSystem = entity.GetParticleSystem world
            let particlesMessages =
                particleSystem |>
                Particles.ParticleSystem.toParticlesDescriptors time |>
                List.map (fun descriptor ->
                    match descriptor with
                    | Particles.BillboardParticlesDescriptor descriptor ->
                        let emitterProperties = entity.GetEmitterMaterialProperties world
                        let materialProperties =
                            { AlbedoOpt = match emitterProperties.AlbedoOpt with ValueSome albedo -> ValueSome albedo | ValueNone -> descriptor.MaterialProperties.AlbedoOpt
                              RoughnessOpt = match emitterProperties.RoughnessOpt with ValueSome roughness -> ValueSome roughness | ValueNone -> descriptor.MaterialProperties.RoughnessOpt
                              MetallicOpt = match emitterProperties.MetallicOpt with ValueSome metallic -> ValueSome metallic | ValueNone -> descriptor.MaterialProperties.MetallicOpt
                              AmbientOcclusionOpt = match emitterProperties.AmbientOcclusionOpt with ValueSome ambientOcclusion -> ValueSome ambientOcclusion | ValueNone -> descriptor.MaterialProperties.AmbientOcclusionOpt
                              EmissionOpt = match emitterProperties.EmissionOpt with ValueSome emission -> ValueSome emission | ValueNone -> descriptor.MaterialProperties.EmissionOpt
                              HeightOpt = match emitterProperties.HeightOpt with ValueSome height -> ValueSome height | ValueNone -> descriptor.MaterialProperties.HeightOpt
                              InvertRoughnessOpt = match emitterProperties.InvertRoughnessOpt with ValueSome invertRoughness -> ValueSome invertRoughness | ValueNone -> descriptor.MaterialProperties.InvertRoughnessOpt }
                        Some
                            (RenderBillboardParticles
                                { Absolute = descriptor.Absolute
                                  MaterialProperties = materialProperties
                                  AlbedoImage = descriptor.AlbedoImage
                                  RoughnessImage = descriptor.RoughnessImage
                                  MetallicImage = descriptor.MetallicImage
                                  AmbientOcclusionImage = descriptor.AmbientOcclusionImage
                                  EmissionImage = descriptor.EmissionImage
                                  NormalImage = descriptor.NormalImage
                                  HeightImage = descriptor.HeightImage
                                  MinFilterOpt = descriptor.MinFilterOpt
                                  MagFilterOpt = descriptor.MagFilterOpt
                                  RenderType = descriptor.RenderType
                                  Particles = descriptor.Particles })
                    | _ -> None) |>
                List.definitize
            World.enqueueRenderMessages3d particlesMessages world

        override this.RayCast (ray, entity, world) =
            let intersectionOpt = ray.Intersects (entity.GetBounds world)
            if intersectionOpt.HasValue then [|intersectionOpt.Value|]
            else [||]

[<AutoOpen>]
module StaticModelFacetModule =

    type Entity with

        member this.GetStaticModel world : StaticModel AssetTag = this.Get (nameof this.StaticModel) world
        member this.SetStaticModel (value : StaticModel AssetTag) world = this.Set (nameof this.StaticModel) value world
        member this.StaticModel = lens (nameof this.StaticModel) this this.GetStaticModel this.SetStaticModel

    /// Augments an entity with a static model.
    type StaticModelFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.RenderStyle Deferred
             define Entity.StaticModel Assets.Default.StaticModel]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrixOffset = transform.AffineMatrixOffset
            let presence = transform.Presence
            let insetOpt = Option.toValueOption (entity.GetInsetOpt world)
            let properties = entity.GetMaterialProperties world
            let renderType =
                match entity.GetRenderStyle world with
                | Deferred -> DeferredRenderType
                | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
            let staticModel = entity.GetStaticModel world
            World.renderStaticModelFast (absolute, &affineMatrixOffset, presence, insetOpt, &properties, renderType, staticModel, world)

        override this.GetQuickSize (entity, world) =
            let staticModel = entity.GetStaticModel world
            match Metadata.tryGetStaticModelMetadata staticModel with
            | Some staticModelMetadata ->
                let bounds = staticModelMetadata.Bounds
                let boundsExtended = bounds.Combine bounds.Mirror
                boundsExtended.Size
            | None -> base.GetQuickSize (entity, world)

        override this.RayCast (ray, entity, world) =
            let affineMatrixOffset = entity.GetAffineMatrixOffset world
            let inverseMatrixOffset = Matrix4x4.Invert affineMatrixOffset |> snd
            let rayEntity = ray.Transform inverseMatrixOffset
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModelMetadata ->
                let intersectionses =
                    Array.map (fun (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                        let geometry = surface.PhysicallyBasedGeometry
                        let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                        let raySurface = rayEntity.Transform inverse
                        let mutable bounds = geometry.Bounds
                        let boundsIntersectionOpt = raySurface.Intersects bounds
                        if boundsIntersectionOpt.HasValue then
                            raySurface.Intersects (geometry.Indices, geometry.Vertices) |>
                            Seq.map snd' |>
                            Seq.map (fun intersectionEntity -> rayEntity.Origin + rayEntity.Direction * intersectionEntity) |>
                            Seq.map (fun pointEntity -> Vector3.Transform (pointEntity, affineMatrixOffset)) |>
                            Seq.map (fun point -> (point - ray.Origin).Magnitude) |>
                            Seq.toArray
                        else [||])
                        staticModelMetadata.Surfaces
                Array.concat intersectionses
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModelMetadata ->
                let mutable boundsOpt = None
                for surface in staticModelMetadata.Surfaces do
                    let bounds2 = surface.SurfaceBounds.Transform surface.SurfaceMatrix
                    match boundsOpt with
                    | Some (bounds : Box3) -> boundsOpt <- Some (bounds.Combine bounds2)
                    | None -> boundsOpt <- Some bounds2
                match boundsOpt with
                | Some bounds ->
                    let boundsOverflow = bounds.ScaleUniform (entity.GetOverflow world)
                    Some (boundsOverflow.Transform (entity.GetAffineMatrixOffset world))
                | None -> None
            | None -> None

[<AutoOpen>]
module StaticModelSurfaceFacetModule =

    type Entity with
        member this.GetSurfaceIndex world : int = this.Get (nameof this.SurfaceIndex) world
        member this.SetSurfaceIndex (value : int) world = this.Set (nameof this.SurfaceIndex) value world
        member this.SurfaceIndex = lens (nameof this.SurfaceIndex) this this.GetSurfaceIndex this.SetSurfaceIndex

    /// Augments an entity with an indexed static model surface.
    type StaticModelSurfaceFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.RenderStyle Deferred
             define Entity.StaticModel Assets.Default.StaticModel
             define Entity.SurfaceIndex 0]

        override this.Render (entity, world) =
            match entity.GetSurfaceIndex world with
            | -1 -> world
            | surfaceIndex ->
                let mutable transform = entity.GetTransform world
                let absolute = transform.Absolute
                let affineMatrixOffset = transform.AffineMatrixOffset
                let insetOpt = Option.toValueOption (entity.GetInsetOpt world)
                let properties = entity.GetMaterialProperties world
                let renderType =
                    match entity.GetRenderStyle world with
                    | Deferred -> DeferredRenderType
                    | Forward (subsort, sort) -> ForwardRenderType (subsort, sort)
                let staticModel = entity.GetStaticModel world
                World.renderStaticModelSurfaceFast (absolute, &affineMatrixOffset, insetOpt, &properties, renderType, staticModel, surfaceIndex, world)

        override this.GetQuickSize (entity, world) =
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModelMetadata ->
                let surfaceIndex = entity.GetSurfaceIndex world
                if surfaceIndex > -1 && surfaceIndex < staticModelMetadata.Surfaces.Length then
                    let bounds = staticModelMetadata.Surfaces.[surfaceIndex].SurfaceBounds
                    let boundsExtended = bounds.Combine bounds.Mirror
                    boundsExtended.Size
                else base.GetQuickSize (entity, world)
            | None -> base.GetQuickSize (entity, world)

        override this.RayCast (ray, entity, world) =
            let rayEntity = ray.Transform (Matrix4x4.Invert (entity.GetAffineMatrixOffset world) |> snd)
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModelMetadata ->
                let surfaceIndex = entity.GetSurfaceIndex world
                if surfaceIndex < staticModelMetadata.Surfaces.Length then
                    let surface = staticModelMetadata.Surfaces.[surfaceIndex]
                    let geometry = surface.PhysicallyBasedGeometry
                    let mutable bounds = geometry.Bounds
                    let boundsIntersectionOpt = rayEntity.Intersects bounds
                    if boundsIntersectionOpt.HasValue then
                        let intersections = rayEntity.Intersects (geometry.Indices, geometry.Vertices)
                        intersections |> Seq.map snd' |> Seq.toArray
                    else [||]
                else [||]
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            match Metadata.tryGetStaticModelMetadata (entity.GetStaticModel world) with
            | Some staticModelMetadata ->
                let surfaceIndex = entity.GetSurfaceIndex world
                if surfaceIndex < staticModelMetadata.Surfaces.Length then
                    let surface = staticModelMetadata.Surfaces.[surfaceIndex]
                    let bounds = surface.PhysicallyBasedGeometry.Bounds
                    let boundsOverflow = bounds.ScaleUniform (entity.GetOverflow world)
                    Some (boundsOverflow.Transform (entity.GetAffineMatrixOffset world))
                else None
            | None -> None

[<AutoOpen>]
module AnimatedModelFacetModule =

    type Entity with

        member this.GetAnimations world : Animation array = this.Get (nameof this.Animations) world
        member this.SetAnimations (value : Animation array) world = this.Set (nameof this.Animations) value world
        member this.Animations = lens (nameof this.Animations) this this.GetAnimations this.SetAnimations
        member this.GetAnimatedModel world : AnimatedModel AssetTag = this.Get (nameof this.AnimatedModel) world
        member this.SetAnimatedModel (value : AnimatedModel AssetTag) world = this.Set (nameof this.AnimatedModel) value world
        member this.AnimatedModel = lens (nameof this.AnimatedModel) this this.GetAnimatedModel this.SetAnimatedModel

    /// Augments an entity with an animated model.
    type AnimatedModelFacet () =
        inherit Facet (false)

        static member Properties =
            [define Entity.StartTime GameTime.zero
             define Entity.InsetOpt None
             define Entity.MaterialProperties MaterialProperties.defaultProperties
             define Entity.Animations [|{ StartTime = GameTime.zero; LifeTimeOpt = None; Name = "Armature|Idle"; Playback = Loop; Rate = 1.0f; Weight = 1.0f; BoneFilterOpt = None }|]
             define Entity.AnimatedModel Assets.Default.AnimatedModel]

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let absolute = transform.Absolute
            let affineMatrixOffset = transform.AffineMatrixOffset
            let startTime = entity.GetStartTime world
            let localTime = world.GameTime - startTime
            let insetOpt = Option.toValueOption (entity.GetInsetOpt world)
            let properties = entity.GetMaterialProperties world
            let animations = entity.GetAnimations world
            let animatedModel = entity.GetAnimatedModel world
            World.renderAnimatedModelFast (localTime, absolute, &affineMatrixOffset, insetOpt, &properties, animations, animatedModel, world)

        override this.GetQuickSize (entity, world) =
            let animatedModel = entity.GetAnimatedModel world
            match Metadata.tryGetAnimatedModelMetadata animatedModel with
            | Some animatedModelMetadata ->
                let bounds = animatedModelMetadata.Bounds
                let boundsExtended = bounds.Combine bounds.Mirror
                boundsExtended.Size
            | None -> base.GetQuickSize (entity, world)

        override this.RayCast (ray, entity, world) =
            let affineMatrixOffset = entity.GetAffineMatrixOffset world
            let inverseMatrixOffset = Matrix4x4.Invert affineMatrixOffset |> snd
            let rayEntity = ray.Transform inverseMatrixOffset
            match Metadata.tryGetAnimatedModelMetadata (entity.GetAnimatedModel world) with
            | Some animatedModelMetadata ->
                let intersectionses =
                    Array.map (fun (surface : OpenGL.PhysicallyBased.PhysicallyBasedSurface) ->
                        // TODO: include animation state.
                        let geometry = surface.PhysicallyBasedGeometry
                        let (_, inverse) = Matrix4x4.Invert surface.SurfaceMatrix
                        let raySurface = rayEntity.Transform inverse
                        let mutable bounds = geometry.Bounds
                        let boundsIntersectionOpt = raySurface.Intersects bounds
                        if boundsIntersectionOpt.HasValue then
                            raySurface.Intersects (geometry.Indices, geometry.Vertices) |>
                            Seq.map snd' |>
                            Seq.map (fun intersectionEntity -> rayEntity.Origin + rayEntity.Direction * intersectionEntity) |>
                            Seq.map (fun pointEntity -> Vector3.Transform (pointEntity, affineMatrixOffset)) |>
                            Seq.map (fun point -> (point - ray.Origin).Magnitude) |>
                            Seq.toArray
                        else [||])
                        animatedModelMetadata.Surfaces
                Array.concat intersectionses
            | None -> [||]

        override this.TryGetHighlightBounds (entity, world) =
            match Metadata.tryGetAnimatedModelMetadata (entity.GetAnimatedModel world) with
            | Some animatedModelMetadata ->
                let mutable boundsOpt = None
                for surface in animatedModelMetadata.Surfaces do
                    let bounds2 = surface.SurfaceBounds.Transform surface.SurfaceMatrix
                    match boundsOpt with
                    | Some (bounds : Box3) -> boundsOpt <- Some (bounds.Combine bounds2)
                    | None -> boundsOpt <- Some bounds2
                match boundsOpt with
                | Some bounds ->
                    let boundsOverflow = bounds.ScaleUniform (entity.GetOverflow world)
                    Some (boundsOverflow.Transform (entity.GetAffineMatrixOffset world))
                | None -> None
            | None -> None

[<AutoOpen>]
module TerrainFacetModule =

    type Entity with
        member this.GetTerrainMaterialProperties world : TerrainMaterialProperties = this.Get (nameof this.TerrainMaterialProperties) world
        member this.SetTerrainMaterialProperties (value : TerrainMaterialProperties) world = this.Set (nameof this.TerrainMaterialProperties) value world
        member this.TerrainMaterialProperties = lens (nameof this.TerrainMaterialProperties) this this.GetTerrainMaterialProperties this.SetTerrainMaterialProperties
        member this.GetTerrainMaterial world : TerrainMaterial = this.Get (nameof this.TerrainMaterial) world
        member this.SetTerrainMaterial (value : TerrainMaterial) world = this.Set (nameof this.TerrainMaterial) value world
        member this.TerrainMaterial = lens (nameof this.TerrainMaterial) this this.GetTerrainMaterial this.SetTerrainMaterial
        member this.GetTintImage world : Image AssetTag = this.Get (nameof this.TintImage) world
        member this.SetTintImage (value : Image AssetTag) world = this.Set (nameof this.TintImage) value world
        member this.TintImage = lens (nameof this.TintImage) this this.GetTintImage this.SetTintImage
        member this.GetNormalImageOpt world : Image AssetTag option = this.Get (nameof this.NormalImageOpt) world
        member this.SetNormalImageOpt (value : Image AssetTag option) world = this.Set (nameof this.NormalImageOpt) value world
        member this.NormalImageOpt = lens (nameof this.NormalImageOpt) this this.GetNormalImageOpt this.SetNormalImageOpt
        member this.GetTiles world : Vector2 = this.Get (nameof this.Tiles) world
        member this.SetTiles (value : Vector2) world = this.Set (nameof this.Tiles) value world
        member this.Tiles = lens (nameof this.Tiles) this this.GetTiles this.SetTiles
        member this.GetHeightMap world : HeightMap = this.Get (nameof this.HeightMap) world
        member this.SetHeightMap (value : HeightMap) world = this.Set (nameof this.HeightMap) value world
        member this.HeightMap = lens (nameof this.HeightMap) this this.GetHeightMap this.SetHeightMap
        member this.GetSegments world : Vector2i = this.Get (nameof this.Segments) world
        member this.SetSegments (value : Vector2i) world = this.Set (nameof this.Segments) value world
        member this.Segments = lens (nameof this.Segments) this this.GetSegments this.SetSegments

        member this.TryGetTerrainResolution world =
            match this.GetHeightMap world with
            | ImageHeightMap map ->
                match Metadata.tryGetTextureSize map with
                | Some textureSize -> Some textureSize
                | None -> None
            | RawHeightMap map -> Some map.Resolution

        member this.TryGetTerrainQuadSize world =
            let bounds = this.GetBounds world
            match this.TryGetTerrainResolution world with
            | Some resolution -> Some (v2 (bounds.Size.X / single (dec resolution.X)) (bounds.Size.Z / single (dec resolution.Y)))
            | None -> None

    /// Augments an entity with a rigid 3d terrain.
    type TerrainFacet () =
        inherit Facet (true)

        static member Properties =
            [define Entity.Size (v3 512.0f 128.0f 512.0f)
             define Entity.Presence Omnipresent
             define Entity.AlwaysRender true
             define Entity.BodyEnabled true
             define Entity.Friction 0.5f
             define Entity.Restitution 0.0f
             define Entity.CollisionCategories "1"
             define Entity.CollisionMask Constants.Physics.CollisionWildcard
             define Entity.InsetOpt None
             define Entity.TerrainMaterialProperties TerrainMaterialProperties.defaultProperties
             define Entity.TerrainMaterial
                (BlendMaterial
                    { TerrainLayers =
                        [|{ AlbedoImage = Assets.Default.TerrainLayerAlbedo
                            RoughnessImage = Assets.Default.TerrainLayerRoughness
                            AmbientOcclusionImage = Assets.Default.TerrainLayerAmbientOcclusion
                            NormalImage = Assets.Default.TerrainLayerNormal
                            HeightImage = Assets.Default.TerrainLayerHeight }
                          { AlbedoImage = Assets.Default.TerrainLayer2Albedo
                            RoughnessImage = Assets.Default.TerrainLayer2Roughness
                            AmbientOcclusionImage = Assets.Default.TerrainLayer2AmbientOcclusion
                            NormalImage = Assets.Default.TerrainLayer2Normal
                            HeightImage = Assets.Default.TerrainLayer2Height }|]
                      BlendMap =
                          RedsMap
                            [|Assets.Default.TerrainLayerBlend
                              Assets.Default.TerrainLayer2Blend|]})
             define Entity.TintImage Assets.Default.TerrainTint
             define Entity.NormalImageOpt None
             define Entity.Tiles (v2 256.0f 256.0f)
             define Entity.HeightMap (RawHeightMap { Resolution = v2i 513 513; RawFormat = RawUInt16 LittleEndian; RawAsset = Assets.Default.HeightMap })
             define Entity.Segments v2iOne
             computed Entity.BodyId (fun (entity : Entity) _ -> { BodySource = entity; BodyIndex = 0 }) None]

        override this.Register (entity, world) =
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.BodyEnabled)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Transform)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Friction)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.Restitution)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionCategories)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.CollisionMask)) entity (nameof TerrainFacet) world
            let world = World.sense (fun _ world -> (Cascade, entity.PropagatePhysics world)) (entity.ChangeEvent (nameof entity.HeightMap)) entity (nameof TerrainFacet) world
            world

        override this.RegisterPhysics (entity, world) =
            match entity.TryGetTerrainResolution world with
            | Some resolution ->
                let mutable transform = entity.GetTransform world
                let bodyTerrain =
                    { Resolution = resolution
                      Bounds = transform.Bounds
                      HeightMap = entity.GetHeightMap world
                      TransformOpt = None
                      PropertiesOpt = None }
                let bodyProperties =
                    { BodyIndex = (entity.GetBodyId world).BodyIndex
                      Center = transform.Center
                      Rotation = transform.Rotation
                      BodyShape = BodyTerrain bodyTerrain
                      BodyType = Static
                      SleepingAllowed = true
                      Enabled = entity.GetBodyEnabled world
                      Friction = entity.GetFriction world
                      Restitution = entity.GetRestitution world
                      LinearVelocity = v3Zero
                      LinearDamping = 0.0f
                      AngularVelocity = v3Zero
                      AngularDamping = 0.0f
                      AngularFactor = v3Zero
                      Substance = Mass 0.0f
                      GravityOverride = None
                      CollisionDetection = Discontinuous
                      CollisionCategories = Physics.categorizeCollisionMask (entity.GetCollisionCategories world)
                      CollisionMask = Physics.categorizeCollisionMask (entity.GetCollisionMask world)
                      Sensor = false }
                World.createBody false (entity.GetBodyId world) bodyProperties world
            | None -> world

        override this.UnregisterPhysics (entity, world) =
            World.destroyBody false (entity.GetBodyId world) world

        override this.Render (entity, world) =
            let mutable transform = entity.GetTransform world
            let terrainDescriptor =
                { Bounds = transform.Bounds
                  InsetOpt = entity.GetInsetOpt world
                  MaterialProperties = entity.GetTerrainMaterialProperties world
                  Material = entity.GetTerrainMaterial world
                  TintImage = entity.GetTintImage world
                  NormalImageOpt = entity.GetNormalImageOpt world
                  Tiles = entity.GetTiles world
                  HeightMap = entity.GetHeightMap world
                  Segments = entity.GetSegments world }
            World.enqueueRenderMessage3d
                (RenderTerrain
                    { Absolute = transform.Absolute
                      Visible = transform.Visible
                      TerrainDescriptor = terrainDescriptor })
                world

        override this.GetQuickSize (entity, world) =
            match entity.TryGetTerrainResolution world with
            | Some resolution -> v3 (single (dec resolution.X)) 128.0f (single (dec resolution.Y))
            | None -> v3 512.0f 128.0f 512.0f