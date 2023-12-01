﻿// Gaia - The Nu Game Engine editor.
// Copyright (C) Bryan Edds, 2013-2023.

namespace Nu.Gaia
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Reflection
open FSharp.Compiler.Interactive
open FSharp.NativeInterop
open FSharp.Reflection
open Microsoft.FSharp.Core
open ImGuiNET
open ImGuizmoNET
open Prime
open Nu

///////////////////////////////////
// TODO:
//
// Paste in hierarchy.
// Log Output window.
// Sense functions for Stream.
// Perhaps look up some-constructed default property values from overlayer.
//
// Custom properties in order of priority:
//
//  Enums
//  Layout
//  CollisionMask
//  CollisionCategories
//  CollisionDetection
//  BodyShape
//  JointDevice
//  DateTimeOffset?
//  SymbolicCompression
//  Flag Enums
//
///////////////////////////////////

[<RequireQualifiedAccess>]
module Gaia =

    (* World States - mutable to accomodatae ImGui's intended authoring style *)

    let mutable private world = Unchecked.defaultof<World> // this will be initialized on start
    let mutable private worldsPast = []
    let mutable private worldsFuture = []

    (* Active Editing States *)

    let mutable private manipulationActive = false
    let mutable private manipulationOperation = OPERATION.TRANSLATE
    let mutable private expandEntityHierarchy = false
    let mutable private collapseEntityHierarchy = false
    let mutable private showSelectedEntity = false
    let mutable private rightClickPosition = v2Zero
    let mutable private focusedPropertyDescriptorOpt = Option<PropertyDescriptor * Simulant>.None
    let mutable private focusPropertyEditorRequested = false
    let mutable private propertyValueStrPrevious = ""
    let mutable private dragDropPayloadOpt = None
    let mutable private dragEntityState = DragEntityInactive
    let mutable private dragEyeState = DragEyeInactive
    let mutable private selectedScreen = Game / "Screen" // TODO: see if this is necessary or if we can just use World.getSelectedScreen.
    let mutable private selectedGroup = selectedScreen / "Group"
    let mutable private selectedEntityOpt = Option<Entity>.None
    let mutable private openProjectFilePath = null // this will be initialized on start
    let mutable private openProjectEditMode = "Title"
    let mutable private openProjectImperativeExecution = false
    let mutable private newProjectName = "MyGame"
    let mutable private newGroupDispatcherName = nameof GroupDispatcher
    let mutable private newEntityDispatcherName = null // this will be initialized on start
    let mutable private newEntityOverlayName = "(Default Overlay)"
    let mutable private newEntityElevation = 0.0f
    let mutable private newGroupName = ""
    let mutable private groupRename = ""
    let mutable private entityRename = ""
    let mutable private desiredEyeCenter2d = v2Zero
    let mutable private desiredEyeCenter3d = v3Zero
    let mutable private desiredEyeRotation3d = quatIdentity

    (* Configuration States *)

    let mutable private fullScreen = false
    let mutable private editWhileAdvancing = false
    let mutable private snaps2dSelected = false
    let mutable private snaps2d = (Constants.Gaia.Position2dSnapDefault, Constants.Gaia.Degrees2dSnapDefault, Constants.Gaia.Scale2dSnapDefault)
    let mutable private snaps3d = (Constants.Gaia.Position3dSnapDefault, Constants.Gaia.Degrees3dSnapDefault, Constants.Gaia.Scale3dSnapDefault)
    let mutable private snapDrag = 0.1f
    let mutable private assetViewerSearchStr = ""
    let mutable private assetPickerSearchStr = ""
    let mutable private lightMappingConfig = { LightMappingEnabled = true }
    let mutable private ssaoConfig =
        { SsaoEnabled = true
          SsaoIntensity = Constants.Render.SsaoIntensityDefault
          SsaoBias = Constants.Render.SsaoBiasDefault
          SsaoRadius = Constants.Render.SsaoRadiusDefault
          SsaoDistanceMax = Constants.Render.SsaoDistanceMaxDefault
          SsaoSampleCount = Constants.Render.SsaoSampleCountDefault }

    (* Project States *)

    let mutable private targetDir = "."
    let mutable private projectDllPath = ""
    let mutable private projectFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private projectEditMode = ""
    let mutable private projectImperativeExecution = false
    let mutable private groupFileDialogState : ImGuiFileDialogState = null // this will be initialized on start
    let mutable private groupFilePaths = Map.empty<Group Address, string>
    let mutable private assetGraphStr = null // this will be initialized on start
    let mutable private overlayerStr = null // this will be initialized on start

    (* Modal Activity States *)

    let mutable private messageBoxOpt = Option<string>.None
    let mutable private recoverableExceptionOpt = Option<Exception * World>.None
    let mutable private showEntityContextMenu = false
    let mutable private showAssetPickerDialog = false
    let mutable private showNewProjectDialog = false
    let mutable private showOpenProjectDialog = false
    let mutable private showOpenProjectFileDialog = false
    let mutable private showCloseProjectDialog = false
    let mutable private showNewGroupDialog = false
    let mutable private showOpenGroupDialog = false
    let mutable private showSaveGroupDialog = false
    let mutable private showRenameGroupDialog = false
    let mutable private showRenameEntityDialog = false
    let mutable private showConfirmExitDialog = false
    let mutable private showRestartDialog = false
    let mutable private showInspector = false
    let mutable private reloadAssetsRequested = 0
    let mutable private reloadCodeRequested = 0
    let mutable private reloadAllRequested = 0
    let modal () =
        messageBoxOpt.IsSome ||
        recoverableExceptionOpt.IsSome ||
        showEntityContextMenu ||
        showAssetPickerDialog ||
        showNewProjectDialog ||
        showOpenProjectDialog ||
        showOpenProjectFileDialog ||
        showCloseProjectDialog ||
        showNewGroupDialog ||
        showOpenGroupDialog ||
        showSaveGroupDialog ||
        showRenameGroupDialog ||
        showRenameEntityDialog ||
        showConfirmExitDialog ||
        showRestartDialog ||
        reloadAssetsRequested <> 0 ||
        reloadCodeRequested <> 0 ||
        reloadAllRequested <> 0

    (* Memoization *)
    let mutable toSymbolMemo = new ForgetfulDictionary<struct (Type * obj), Symbol> (HashIdentity.FromFunctions hash objEq)
    let mutable ofSymbolMemo = new ForgetfulDictionary<struct (Type * Symbol), obj> (HashIdentity.Structural)

    (* Initial imgui.ini File Content *)

    let private ImGuiIniFileStr = """
[Window][Gaia]
Pos=0,0
Size=1920,54
Collapsed=0
DockId=0x00000002,0

[Window][Property Editor]
Pos=284,854
Size=667,226
Collapsed=0
DockId=0x00000001,0

[Window][Asset Viewer]
Pos=0,56
Size=282,1024
Collapsed=0
DockId=0x0000000C,1

[Window][Asset Graph]
Pos=953,854
Size=669,226
Collapsed=0
DockId=0x00000009,2

[Window][Overlayer]
Pos=953,854
Size=669,226
Collapsed=0
DockId=0x00000009,3

[Window][Event Tracing]
Pos=953,854
Size=669,226
Collapsed=0
DockId=0x00000009,4

[Window][Renderer]
Pos=953,854
Size=669,226
Collapsed=0
DockId=0x00000009,1

[Window][Audio Player]
Pos=953,854
Size=669,226
Collapsed=0
DockId=0x00000009,0

[Window][Full Screen Enabled]
Pos=20,23
Size=162,54
Collapsed=0

[Window][Message!]
Pos=827,398
Size=360,182
Collapsed=0

[Window][Unhandled Exception!]
Pos=608,366
Size=694,406
Collapsed=0

[Window][Are you okay with exiting Gaia?]
Pos=836,504
Size=248,72
Collapsed=0

[Window][ContextMenu]
Pos=853,391
Size=250,135
Collapsed=0

[Window][Viewport]
Pos=0,0
Size=1920,1080
Collapsed=0

[Window][Entity Properties]
Pos=1624,56
Size=296,1024
Collapsed=0
DockId=0x0000000E,3

[Window][Group Properties]
Pos=1624,56
Size=296,1024
Collapsed=0
DockId=0x0000000E,2

[Window][Screen Properties]
Pos=1624,56
Size=296,1024
Collapsed=0
DockId=0x0000000E,1

[Window][Game Properties]
Pos=1624,56
Size=296,1024
Collapsed=0
DockId=0x0000000E,0

[Window][Console]
Pos=998,874
Size=624,206
Collapsed=0
DockId=0x00000009,6

[Window][Entity Hierarchy]
Pos=0,56
Size=282,1024
Collapsed=0
DockId=0x0000000C,0

[Window][Create Nu Project... *EDITOR RESTART REQUIRED!*]
Pos=661,488
Size=621,105
Collapsed=0

[Window][Choose a project .dll... *EDITOR RESTART REQUIRED!*]
Pos=628,476
Size=674,128
Collapsed=0

[Window][Create a group...]
Pos=715,469
Size=482,128
Collapsed=0

[Window][Choose a nugroup file...]
Pos=602,352
Size=677,399
Collapsed=0

[Window][Save a nugroup file...]
Pos=613,358
Size=664,399
Collapsed=0

[Window][Choose an Asset...]
Pos=796,323
Size=336,458
Collapsed=0

[Window][Rename group...]
Pos=734,514
Size=444,94
Collapsed=0

[Window][Rename entity...]
Pos=734,514
Size=444,94
Collapsed=0

[Window][Reloading assets...]
Pos=700,500
Size=520,110
Collapsed=0

[Window][Reloading code...]
Pos=700,500
Size=520,110
Collapsed=0

[Window][Reloading assets and code...]
Pos=700,500
Size=520,110
Collapsed=0

[Window][DockSpaceViewport_11111111]
Pos=0,0
Size=1920,1080
Collapsed=0

[Window][Debug##Default]
Pos=60,60
Size=400,400
Collapsed=0

[Docking][Data]
DockSpace             ID=0x8B93E3BD Window=0xA787BDB4 Pos=0,0 Size=1920,1080 Split=Y
  DockNode            ID=0x00000002 Parent=0x8B93E3BD SizeRef=1920,54 HiddenTabBar=1 Selected=0x48908BE7
  DockNode            ID=0x0000000F Parent=0x8B93E3BD SizeRef=1920,1024 Split=X
    DockNode          ID=0x0000000D Parent=0x0000000F SizeRef=1622,1080 Split=X
      DockNode        ID=0x00000007 Parent=0x0000000D SizeRef=282,1080 Split=X Selected=0x29EABFBD
        DockNode      ID=0x0000000B Parent=0x00000007 SizeRef=174,1022 Selected=0x29EABFBD
        DockNode      ID=0x0000000C Parent=0x00000007 SizeRef=171,1022 Selected=0xAE464409
      DockNode        ID=0x00000008 Parent=0x0000000D SizeRef=1338,1080 Split=X
        DockNode      ID=0x00000005 Parent=0x00000008 SizeRef=1223,979 Split=Y
          DockNode    ID=0x00000004 Parent=0x00000005 SizeRef=1678,796 CentralNode=1
          DockNode    ID=0x00000003 Parent=0x00000005 SizeRef=1678,226 Split=X Selected=0xD4E24632
            DockNode  ID=0x00000001 Parent=0x00000003 SizeRef=667,205 Selected=0x61D81DE4
            DockNode  ID=0x00000009 Parent=0x00000003 SizeRef=669,205 Selected=0xD4E24632
        DockNode      ID=0x00000006 Parent=0x00000008 SizeRef=346,979 Selected=0x199AB496
    DockNode          ID=0x0000000E Parent=0x0000000F SizeRef=296,1080 Selected=0xD5116FF8

"""

    (* Prelude Functions *)

    let private canEditWithMouse () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureMousePlus) && (world.Halted || editWhileAdvancing)

    let private canEditWithKeyboard () =
        let io = ImGui.GetIO ()
        not (io.WantCaptureKeyboardPlus) && (world.Halted || editWhileAdvancing)

    let private snapshot () =
        world <- Nu.World.shelveCurrent world
        worldsPast <- world :: worldsPast
        worldsFuture <- []

    let private containsProperty propertyDescriptor simulant =
        SimulantPropertyDescriptor.containsPropertyDescriptor propertyDescriptor simulant world

    let private getPropertyValue propertyDescriptor simulant =
        SimulantPropertyDescriptor.getValue propertyDescriptor simulant world

    let private setPropertyValueWithoutUndo (value : obj) propertyDescriptor simulant =
        match SimulantPropertyDescriptor.trySetValue value propertyDescriptor simulant world with
        | Right wtemp -> world <- wtemp
        | Left (error, wtemp) -> messageBoxOpt <- Some error; world <- wtemp

    let private setPropertyValueIgnoreError (value : obj) propertyDescriptor simulant =
        snapshot ()
        match SimulantPropertyDescriptor.trySetValue value propertyDescriptor simulant world with
        | Right wtemp -> world <- wtemp
        | Left (_, wtemp) -> world <- wtemp

    let private setPropertyValue (value : obj) propertyDescriptor simulant =
        if  not (ImGui.IsMouseDragging ImGuiMouseButton.Left) ||
            not (ImGui.IsMouseDraggingContinued ImGuiMouseButton.Left) then
            snapshot ()
        setPropertyValueWithoutUndo value propertyDescriptor simulant

    let private selectScreen screen =
        if screen <> selectedScreen then
            ImGui.SetWindowFocus null
            selectedScreen <- screen

    let private selectGroup group =
        if group <> selectedGroup then
            ImGui.SetWindowFocus null
            selectedGroup <- group

    let private selectGroupInitial screen =
        let groups = World.getGroups screen world
        let group =
            match Seq.tryFind (fun (group : Group) -> group.Name = "Scene") groups with // NOTE: try to get the Scene group since it's more likely to be the group the user wants to edit.
            | Some group -> group
            | None ->
                match Seq.tryHead groups with
                | Some group -> group
                | None ->
                    let (group, wtemp) = World.createGroup (Some "Group") screen world in world <- wtemp
                    group
        selectGroup group

    let private selectEntityOpt entityOpt =

        if entityOpt <> selectedEntityOpt then

            // try to focus on same entity property
            match focusedPropertyDescriptorOpt with
            | Some (propertyDescriptor, :? Entity) ->
                match entityOpt with
                | Some entity when containsProperty propertyDescriptor entity ->
                    focusedPropertyDescriptorOpt <- Some (propertyDescriptor, entity)
                | Some _ | None -> focusedPropertyDescriptorOpt <- None
            | Some _ | None -> ()

            // HACK: in order to keep the property of one simulant from being copied to another when the selected
            // simulant is changed, we have to move focus away from the property windows. We chose to focus on the
            // "Entity Hierarchy" window in order to avoid disrupting drag and drop when selecting a different entity
            // in it. Then if there is no entity selected, we'll select the viewport instead
            ImGui.SetWindowFocus "Entity Hierarchy"
            if entityOpt.IsNone then ImGui.SetWindowFocus "Viewport"

        // actually set the selection
        selectedEntityOpt <- entityOpt

    let private tryUndo () =
        if
            (if not (Nu.World.getImperative world) then
                match worldsPast with
                | worldPast :: worldsPast' ->
                    let worldFuture = Nu.World.shelveCurrent world
                    world <- Nu.World.unshelve worldPast
                    worldsPast <- worldsPast'
                    worldsFuture <- worldFuture :: worldsFuture
                    true
                | [] -> false
             else false) then
            propertyValueStrPrevious <- ""
            selectedScreen <- World.getSelectedScreen world
            if not (selectedGroup.Exists world) || not (selectedGroup.Selected world) then
                let group = Seq.head (World.getGroups selectedScreen world)
                selectGroup group
            match selectedEntityOpt with
            | Some entity when not (entity.Exists world) || entity.Group <> selectedGroup -> selectEntityOpt None
            | Some _ | None -> ()
            true
        else false

    let private tryRedo () =
        if
            (if not (Nu.World.getImperative world) then
                match worldsFuture with
                | worldFuture :: worldsFuture' ->
                    let worldPast = Nu.World.shelveCurrent world
                    world <- Nu.World.unshelve worldFuture
                    worldsPast <- worldPast :: worldsPast
                    worldsFuture <- worldsFuture'
                    true
                | [] -> false
             else false) then
            propertyValueStrPrevious <- ""
            selectedScreen <- World.getSelectedScreen world
            if not (selectedGroup.Exists world) || not (selectedGroup.Selected world) then
                let group = Seq.head (World.getGroups selectedScreen world)
                selectGroup group
            match selectedEntityOpt with
            | Some entity when not (entity.Exists world) || entity.Group <> selectedGroup -> selectEntityOpt None
            | Some _ | None -> ()
            true
        else false

    let private markLightProbesStale () =
        let groups = World.getGroups selectedScreen world
        let lightProbes =
            groups |>
            Seq.map (fun group -> World.getEntitiesFlattened group world) |>
            Seq.concat |>
            Seq.filter (fun entity -> entity.Has<LightProbeFacet3d> world)
        for lightProbe in lightProbes do
            world <- lightProbe.SetProbeStale true world

    let private getSnaps () =
        if snaps2dSelected
        then snaps2d
        else snaps3d

    let private getPickCandidates2d () =
        let (entities, wtemp) = World.getEntitiesInView2d (HashSet ()) world in world <- wtemp
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible world) |> Seq.toArray
        entitiesInGroup

    let private getPickCandidates3d () =
        let (entities, wtemp) = World.getEntitiesInView3d (HashSet ()) world in world <- wtemp
        let entitiesInGroup = entities |> Seq.filter (fun entity -> entity.Group = selectedGroup && entity.GetVisible world) |> Seq.toArray
        entitiesInGroup

    let rec private generateEntityName3 dispatcherName existingEntityNames =
        let mutable name = Gen.nameForEditor dispatcherName
        if Set.contains name existingEntityNames
        then generateEntityName3 dispatcherName existingEntityNames
        else name

    let private generateEntityName dispatcherName =
        let existingEntityNames =
            World.getEntitiesFlattened selectedGroup world |>
            Seq.map (fun entity -> entity.Name) |>
            Set.ofSeq
        generateEntityName3 dispatcherName existingEntityNames

    let private tryMousePick mousePosition =
        let entities2d = getPickCandidates2d ()
        let pickedOpt = World.tryPickEntity2d mousePosition entities2d world
        match pickedOpt with
        | Some entity ->
            selectEntityOpt (Some entity)
            Some (0.0f, entity)
        | None ->
            let entities3d = getPickCandidates3d ()
            let pickedOpt = World.tryPickEntity3d mousePosition entities3d world
            match pickedOpt with
            | Some (intersection, entity) ->
                selectEntityOpt (Some entity)
                Some (intersection, entity)
            | None -> None

    (* Nu Event Handlers *)

    let private handleNuMouseButton (_ : Event<MouseButtonData, Game>) wtemp =
        world <- wtemp
        if canEditWithMouse ()
        then (Resolve, world)
        else (Cascade, world)

    let private handleNuSelectedScreenOptChange (evt : Event<ChangeData, Game>) wtemp =
        world <- wtemp
        match evt.Data.Value :?> Screen option with
        | Some screen ->
            selectScreen screen
            selectGroupInitial screen
            selectEntityOpt None
            (Cascade, world)
        | None ->
            // just keep current group selection and screen if no screen selected
            (Cascade, world)

    let private handleNuRender (_ : Event<unit, Game>) wtemp =

        // render light probes of the selected group in play
        world <- wtemp
        let (entities, wtemp) = World.getLightProbesInPlay3d (HashSet ()) world in world <- wtemp
        let lightProbeModels =
            entities |>
            Seq.filter (fun entity -> entity.Group = selectedGroup) |>
            Seq.map (fun light -> (light.GetAffineMatrixOffset world, Omnipresent, None, MaterialProperties.defaultProperties)) |>
            SList.ofSeq
        world <-
            World.enqueueRenderMessage3d
                (RenderStaticModels
                    { Absolute = false
                      StaticModels = lightProbeModels
                      RenderType = ForwardRenderType (0.0f, Single.MinValue / 2.0f)
                      StaticModel = Assets.Default.LightbulbModel })
                world

        // render lights of the selected group in play
        world <- wtemp
        let (entities, wtemp) = World.getLightsInPlay3d (HashSet ()) world in world <- wtemp
        let lightModels =
            entities |>
            Seq.filter (fun entity -> entity.Group = selectedGroup) |>
            Seq.map (fun light -> (light.GetAffineMatrixOffset world, Omnipresent, None, MaterialProperties.defaultProperties)) |>
            SList.ofSeq
        world <-
            World.enqueueRenderMessage3d
                (RenderStaticModels
                    { Absolute = false
                      StaticModels = lightModels
                      RenderType = ForwardRenderType (0.0f, Single.MinValue / 2.0f)
                      StaticModel = Assets.Default.LightbulbModel })
                world

        // render selection highlights
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            let absolute = entity.GetAbsolute world
            let bounds = entity.GetHighlightBounds world
            if entity.GetIs2d world then
                let elevation = Single.MaxValue
                let transform = Transform.makePerimeter bounds v3Zero elevation absolute false
                let image = Assets.Default.HighlightImage
                world <-
                    World.enqueueRenderMessage2d
                        (LayeredOperation2d
                            { Elevation = elevation
                              Horizon = bounds.Bottom.Y
                              AssetTag = AssetTag.generalize image
                              RenderOperation2d =
                                RenderSprite
                                    { Transform = transform
                                      InsetOpt = ValueNone
                                      Image = image
                                      Color = Color.One
                                      Blend = Transparent
                                      Emission = Color.Zero
                                      Flip = FlipNone }})
                        world
            else
                let mutable boundsMatrix = Matrix4x4.CreateScale (bounds.Size + v3Dup 0.01f) // slightly bigger to eye to prevent z-fighting with selected entity
                boundsMatrix.Translation <- bounds.Center
                world <-
                    World.enqueueRenderMessage3d
                        (RenderStaticModel
                            { Absolute = absolute
                              ModelMatrix = boundsMatrix
                              Presence = Omnipresent
                              InsetOpt = None
                              MaterialProperties = MaterialProperties.defaultProperties
                              RenderType = ForwardRenderType (0.0f, Single.MinValue)
                              StaticModel = Assets.Default.HighlightModel })
                        world
        | Some _ | None -> ()

        // fin
        (Cascade, world)

    (* Editor Commands *)

    let private createEntity atMouse inHierarchy =
        snapshot ()
        let dispatcherName = newEntityDispatcherName
        let overlayNameDescriptor =
            match newEntityOverlayName with
            | "(Default Overlay)" -> DefaultOverlay
            | "(Routed Overlay)" -> RoutedOverlay
            | "(No Overlay)" -> NoOverlay
            | overlayName -> ExplicitOverlay overlayName
        let name = generateEntityName dispatcherName
        let surnames =
            match selectedEntityOpt with
            | Some entity when entity.Exists world && inHierarchy -> Array.add name entity.Surnames
            | Some _ | None -> [|name|]
        let (entity, wtemp) = World.createEntity5 dispatcherName overlayNameDescriptor (Some surnames) selectedGroup world in world <- wtemp
        let (positionSnap, degreesSnap, scaleSnap) = getSnaps ()
        let viewport = World.getViewport world
        let mutable entityTransform = entity.GetTransform world
        if entity.GetIs2d world then
            let eyeCenter = World.getEyeCenter2d world
            let eyeSize = World.getEyeSize2d world
            let entityPosition =
                if atMouse
                then viewport.MouseToWorld2d (entity.GetAbsolute world, rightClickPosition, eyeCenter, eyeSize)
                else viewport.MouseToWorld2d (entity.GetAbsolute world, World.getEyeSize2d world, eyeCenter, eyeSize)
            entityTransform.Position <- entityPosition.V3
            entityTransform.Size <- entity.GetQuickSize world
            entityTransform.Elevation <- newEntityElevation
            if snaps2dSelected && ImGui.IsCtrlReleased ()
            then world <- entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform world
            else world <- entity.SetTransform entityTransform world
        else
            let eyeCenter = World.getEyeCenter3d world
            let eyeRotation = World.getEyeRotation3d world
            let entityPosition =
                if atMouse then
                    let ray = viewport.MouseToWorld3d (entity.GetAbsolute world, rightClickPosition, eyeCenter, eyeRotation)
                    let forward = Vector3.Transform (v3Forward, eyeRotation)
                    let plane = plane3 (eyeCenter + forward * Constants.Engine.EyeCenter3dOffset.Z) -forward
                    (ray.Intersection plane).Value
                else eyeCenter + Vector3.Transform (v3Forward, eyeRotation) * Constants.Engine.EyeCenter3dOffset.Z
            entityTransform.Position <- entityPosition
            entityTransform.Size <- entity.GetQuickSize world
            if not snaps2dSelected && ImGui.IsCtrlReleased ()
            then world <- entity.SetTransformSnapped positionSnap degreesSnap scaleSnap entityTransform world
            else world <- entity.SetTransform entityTransform world
        if inHierarchy then
            world <- entity.SetMountOptWithAdjustment (Some (Relation.makeParent ())) world
        match entity.TryGetProperty (nameof entity.ProbeBounds) world with
        | Some property when property.PropertyType = typeof<Box3> ->
            let bounds =
                box3
                    (v3Dup Constants.Render.LightProbeSizeDefault * -0.5f + entity.GetPosition world)
                    (v3Dup Constants.Render.LightProbeSizeDefault)
            world <- entity.SetProbeBounds bounds world
        | Some _ | None -> ()
        selectEntityOpt (Some entity)
        ImGui.SetWindowFocus "Viewport"
        showSelectedEntity <- true

    let private trySaveSelectedGroup filePath =
        try World.writeGroupToFile filePath selectedGroup world
            try let deploymentPath = Path.Combine (targetDir, (Path.GetRelativePath (targetDir, filePath)).Replace ("..\\", ""))
                if Directory.Exists (Path.GetDirectoryName deploymentPath) then
                    File.Copy (filePath, deploymentPath, true)
            with exn ->
                messageBoxOpt <- Some ("Could not deploy file due to: " + scstring exn)
            groupFilePaths <- Map.add selectedGroup.GroupAddress groupFileDialogState.FilePath groupFilePaths
            true
        with exn ->
            messageBoxOpt <- Some ("Could not save file due to: " + scstring exn)
            false

    let private tryLoadSelectedGroup filePath =

        // ensure group isn't protected
        if not (selectedGroup.GetProtected world) then

            // attempt to load group descriptor
            let groupAndDescriptorOpt =
                try let groupDescriptorStr = File.ReadAllText filePath
                    let groupDescriptor = scvalue<GroupDescriptor> groupDescriptorStr
                    let groupName =
                        match groupDescriptor.GroupProperties.TryFind Constants.Engine.NamePropertyName with
                        | Some (Atom (name, _)) -> name
                        | _ -> failwithumf ()
                    Right (selectedScreen / groupName, groupDescriptor)
                with exn ->
                    Left exn

            // attempt to load group
            match groupAndDescriptorOpt with
            | Right (group, groupDescriptor) ->
                let worldOld = world
                try if group.Exists world then
                        world <- World.destroyGroupImmediate selectedGroup world
                    let (group, wtemp) = World.readGroup groupDescriptor None selectedScreen world in world <- wtemp
                    selectGroup group
                    match selectedEntityOpt with
                    | Some entity when not (entity.Exists world) || entity.Group <> selectedGroup -> selectEntityOpt None
                    | Some _ | None -> ()
                    groupFilePaths <- Map.add group.GroupAddress groupFileDialogState.FilePath groupFilePaths
                    groupFileDialogState.FileName <- ""
                    true
                with exn ->
                    world <- World.choose worldOld
                    messageBoxOpt <- Some ("Could not load group file due to: " + scstring exn)
                    false

            // error
            | Left exn ->
                messageBoxOpt <- Some ("Could not load group file due to: " + scstring exn)
                false

        // error
        else
            messageBoxOpt <- Some "Cannot load into a protected simulant (such as a group created by the MMCC API)."
            false

    let private tryQuickSizeSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            snapshot ()
            world <- entity.SetSize (entity.GetQuickSize world) world
            true
        | Some _ | None -> false

    let private tryReorderSelectedEntity up =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            let peerOpt =
                if up
                then World.tryGetPreviousEntity entity world
                else World.tryGetNextEntity entity world
            match peerOpt with
            | Some peer ->
                if not (entity.GetProtected world) && not (peer.GetProtected world)
                then world <- World.swapEntityOrders entity peer world
                else messageBoxOpt <- Some "Cannot reorder a protected simulant (such as an entity created by the MMCC API)."
            | None -> ()
        | Some _ | None -> ()

    let private tryDeleteSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            if not (entity.GetProtected world) then
                snapshot ()
                world <- World.destroyEntity entity world
                true
            else
                messageBoxOpt <- Some "Cannot destroy a protected simulant (such as an entity created by the MMCC API)."
                false
        | Some _ | None -> false

    let private tryCutSelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            if not (entity.GetProtected world) then
                snapshot ()
                selectEntityOpt None
                world <- World.cutEntityToClipboard entity world
                true
            else
                messageBoxOpt <- Some "Cannot cut a protected simulant (such as an entity created by the MMCC API)."
                false
        | Some _ | None -> false

    let private tryCopySelectedEntity () =
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            World.copyEntityToClipboard entity world
            true
        | Some _ | None -> false

    let private tryPaste atMouse =
        snapshot ()
        let surnamesOpt =
            World.tryGetEntityDispatcherNameOnClipboard world |>
            Option.map (fun dispatcherName -> generateEntityName dispatcherName) |>
            Option.map Array.singleton
        let snapsEir = if snaps2dSelected then Left snaps2d else Right snaps3d
        let (entityOpt, wtemp) = World.pasteEntityFromClipboard atMouse rightClickPosition snapsEir surnamesOpt selectedGroup world in world <- wtemp
        match entityOpt with
        | Some entity ->
            selectEntityOpt (Some entity)
            ImGui.SetWindowFocus "Viewport"
            showSelectedEntity <- true
            true
        | None -> false

    let private trySetSelectedEntityFamilyStatic static_ =
        let rec setEntityFamilyStatic static_ (entity : Entity) =
            world <- entity.SetStatic static_ world
            for entity in entity.GetChildren world do
                setEntityFamilyStatic static_ entity
        match selectedEntityOpt with
        | Some entity when entity.Exists world ->
            snapshot ()
            setEntityFamilyStatic static_ entity
        | Some _ | None -> ()

    let private tryReloadAssets () =
        let assetSourceDir = targetDir + "/../../.."
        match World.tryReloadAssetGraph assetSourceDir targetDir Constants.Engine.RefinementDir world with
        | (Right assetGraph, wtemp) ->
            world <- wtemp
            let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
            assetGraphStr <- PrettyPrinter.prettyPrint (scstring assetGraph) prettyPrinter
        | (Left error, wtemp) ->
            world <- wtemp
            messageBoxOpt <- Some ("Asset reload error due to: " + error + "'.")
            ()

    let private tryReloadCode () =
        if World.getAllowCodeReload world then
            snapshot ()
            let worldOld = world
            selectEntityOpt None // NOTE: makes sure old dispatcher doesn't hang around in old cached entity state.
            let workingDirPath = targetDir + "/../../.."
            Log.info ("Inspecting directory " + workingDirPath + " for F# code...")
            try match Array.ofSeq (Directory.EnumerateFiles (workingDirPath, "*.fsproj")) with
                | [||] -> Log.trace ("Unable to find fsproj file in '" + workingDirPath + "'.")
                | fsprojFilePaths ->
                    let fsprojFilePath = fsprojFilePaths.[0]
                    Log.info ("Inspecting code for F# project '" + fsprojFilePath + "'...")
                    let fsprojFileLines = File.ReadAllLines fsprojFilePath
                    let fsprojNugetPaths = // imagine manually parsing an xml file...
                        fsprojFileLines |>
                        Array.map (fun line -> line.Trim ()) |>
                        Array.filter (fun line -> line.Contains "PackageReference") |>
                        Array.map (fun line -> line.Replace ("<PackageReference Include=", "nuget: ")) |>
                        Array.map (fun line -> line.Replace (" Version=", ", ")) |>
                        Array.map (fun line -> line.Replace ("/>", "")) |>
                        Array.map (fun line -> line.Replace ("\"", "")) |>
                        Array.map (fun line -> line.Trim ())
                    let fsprojDllFilePaths =
                        fsprojFileLines |>
                        Array.map (fun line -> line.Trim ()) |>
                        Array.filter (fun line -> line.Contains "HintPath" && line.Contains ".dll") |>
                        Array.map (fun line -> line.Replace ("<HintPath>", "")) |>
                        Array.map (fun line -> line.Replace ("</HintPath>", "")) |>
                        Array.map (fun line -> line.Replace ("=", "")) |>
                        Array.map (fun line -> line.Replace ("\"", "")) |>
                        Array.map (fun line -> line.Replace ("\\", "/")) |>
                        Array.map (fun line -> line.Trim ())
                    let fsprojProjectLines = // TODO: see if we can pull these from the fsproj as well...
                        ["#r \"../../../../../Nu/Nu.Math/bin/" + Constants.Gaia.BuildName + "/netstandard2.0/Nu.Math.dll\""
                         "#r \"../../../../../Nu/Nu.Pipe/bin/" + Constants.Gaia.BuildName + "/net7.0/Nu.Pipe.dll\""
                         "#r \"../../../../../Nu/Nu/bin/" + Constants.Gaia.BuildName + "/net7.0/Nu.dll\""]
                    let fsprojFsFilePaths =
                        fsprojFileLines |>
                        Array.map (fun line -> line.Trim ()) |>
                        Array.filter (fun line -> line.Contains "Compile Include" && line.Contains ".fs") |>
                        Array.filter (fun line -> line.Contains "Compile Include" && not (line.Contains "Program.fs")) |>
                        Array.map (fun line -> line.Replace ("<Compile Include", "")) |>
                        Array.map (fun line -> line.Replace ("/>", "")) |>
                        Array.map (fun line -> line.Replace ("=", "")) |>
                        Array.map (fun line -> line.Replace ("\"", "")) |>
                        Array.map (fun line -> line.Replace ("\\", "/")) |>
                        Array.map (fun line -> line.Trim ())
                    let fsxFileString =
                        String.Join ("\n", Array.map (fun (nugetPath : string) -> "#r \"" + nugetPath + "\"") fsprojNugetPaths) + "\n" +
                        String.Join ("\n", Array.map (fun (filePath : string) -> "#r \"../../../" + filePath + "\"") fsprojDllFilePaths) + "\n" +
                        String.Join ("\n", fsprojProjectLines) + "\n" +
                        String.Join ("\n", Array.map (fun (filePath : string) -> "#load \"../../../" + filePath + "\"") fsprojFsFilePaths)
                    let fsProjectNoWarn = "--nowarn:FS9;FS1178;FS3391;FS3536" // TODO: pull these from fsproj!
                    Log.info ("Compiling code via generated F# script:\n" + fsxFileString)
                    let defaultArgs = [|"fsi.exe"; "--debug+"; "--debug:full"; "--optimize-"; "--tailcalls-"; "--multiemit+"; "--gui-"; fsProjectNoWarn|]
                    use errorStream = new StringWriter ()
                    use inStream = new StringReader ""
                    use outStream = new StringWriter ()
                    let fsiConfig = Shell.FsiEvaluationSession.GetDefaultConfiguration ()
                    use session = Shell.FsiEvaluationSession.Create (fsiConfig, defaultArgs, inStream, outStream, errorStream)
                    try session.EvalInteraction fsxFileString
                        let error = string errorStream
                        if error.Length > 0
                        then Log.info ("Code compiled with the following warnings (these may disable debugging of reloaded code):\n" + error)
                        else Log.info "Code compiled with no warnings."
                        Log.info "Updating code..."
                        focusedPropertyDescriptorOpt <- None // drop any reference to old property type
                        world <- World.updateLateBindings session.DynamicAssemblies world // replace references to old types
                        Log.info "Code updated."
                    with _ ->
                        let error = string errorStream
                        Log.trace ("Failed to compile code due to (see full output in the console):\n" + error)
                        world <- World.choose worldOld
            with exn ->
                Log.trace ("Failed to inspect for F# code due to: " + scstring exn)
                world <- World.choose worldOld
        else messageBoxOpt <- Some "Code reloading not allowed by current plugin. This is likely because you're using the GaiaPlugin which doesn't allow it."

    let private tryReloadAll () =
        tryReloadAssets ()
        tryReloadCode ()

    let private resetEye () =
        desiredEyeCenter2d <- v2Zero
        desiredEyeCenter3d <- Constants.Engine.EyeCenter3dDefault
        desiredEyeRotation3d <- quatIdentity

    let private toggleAdvancing () =
        if not world.Advancing then snapshot ()
        world <- World.setAdvancing (not world.Advancing) world

    let private trySelectTargetDirAndMakeNuPluginFromFilePathOpt filePathOpt =
        let gaiaDir = Directory.GetCurrentDirectory ()
        let filePathAndDirNameAndTypesOpt =
            if not (String.IsNullOrWhiteSpace filePathOpt) then
                let filePath = filePathOpt
                try let dirName = Path.GetDirectoryName filePath
                    try Directory.SetCurrentDirectory dirName
                        let assembly = Assembly.Load (File.ReadAllBytes filePath)
                        Right (Some (filePath, dirName, assembly.GetTypes ()))
                    with _ ->
                        let assembly = Assembly.LoadFrom filePath
                        Right (Some (filePath, dirName, assembly.GetTypes ()))
                with _ ->
                    Log.info ("Failed to load Nu game project from '" + filePath + "' due to: " + scstring exn)
                    Directory.SetCurrentDirectory gaiaDir
                    Left ()
            else Right None
        match filePathAndDirNameAndTypesOpt with
        | Right (Some (filePath, dirName, types)) ->
            let pluginTypeOpt = Array.tryFind (fun (ty : Type) -> ty.IsSubclassOf typeof<NuPlugin>) types
            match pluginTypeOpt with
            | Some ty ->
                let plugin = Activator.CreateInstance ty :?> NuPlugin
                Right (Some (filePath, dirName, plugin))
            | None ->
                Directory.SetCurrentDirectory gaiaDir
                Left ()
        | Right None -> Right None
        | Left () -> Left ()

    let private selectNuPlugin gaiaPlugin =
        let gaiaState =
            try if File.Exists Constants.Gaia.StateFilePath
                then scvalue (File.ReadAllText Constants.Gaia.StateFilePath)
                else GaiaState.defaultState
            with _ -> GaiaState.defaultState
        let gaiaDirectory = Directory.GetCurrentDirectory ()
        match trySelectTargetDirAndMakeNuPluginFromFilePathOpt gaiaState.ProjectDllPath with
        | Right (Some (filePath, targetDir, plugin)) ->
            Constants.Override.fromAppConfig filePath
            try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, scstring gaiaState)
            with _ -> Log.info "Could not save gaia state."
            (gaiaState, targetDir, plugin)
        | Right None ->
            try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, scstring gaiaState)
            with _ -> Log.info "Could not save gaia state."
            (gaiaState, ".", gaiaPlugin)
        | Left () ->
            if not (String.IsNullOrWhiteSpace gaiaState.ProjectDllPath) then
                Log.trace ("Invalid Nu Assembly: " + gaiaState.ProjectDllPath)
            (GaiaState.defaultState, ".", gaiaPlugin)

    let private tryMakeWorld sdlDeps worldConfig (plugin : NuPlugin) =

        // attempt to make the world
        match World.tryMake sdlDeps worldConfig plugin with
        | Right world ->

            // initialize event filter as not to flood the log
            let world = World.setEventFilter Constants.Gaia.EventFilter world

            // apply any selected mode
            let world =
                match worldConfig.ModeOpt with
                | Some mode ->
                    match plugin.EditModes.TryGetValue mode with
                    | (true, modeFn) -> modeFn world
                    | (false, _) -> world
                | None -> world

            // figure out which screen to use
            let (screen, world) =
                match Game.GetDesiredScreen world with
                | Desire screen -> (screen, world)
                | DesireNone ->
                    let (screen, world) = World.createScreen (Some "Screen") world
                    let world = Game.SetDesiredScreen (Desire screen) world
                    (screen, world)
                | DesireIgnore ->
                    let (screen, world) = World.createScreen (Some "Screen") world
                    let world = World.setSelectedScreen screen world
                    (screen, world)

            // proceed directly to idle state
            let world = World.selectScreen (IdlingState world.GameTime) screen world
            Right (screen, world)

        // error
        | Left error -> Left error

    let private tryMakeSdlDeps () =
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "MyGame" }
        let sdlConfig = { SdlConfig.defaultConfig with ViewConfig = NewWindow sdlWindowConfig }
        match SdlDeps.tryMake sdlConfig with
        | Left msg -> Left msg
        | Right sdlDeps -> Right (sdlConfig, sdlDeps)

    (* ImGui Callback Functions *)

    let private updateEntityContext () =
        if canEditWithMouse () then
            if ImGui.IsMouseReleased ImGuiMouseButton.Right then
                let mousePosition = World.getMousePosition world
                let _ = tryMousePick mousePosition
                rightClickPosition <- mousePosition
                showEntityContextMenu <- true

    let private updateEntityDrag () =

        if canEditWithMouse () then
        
            if ImGui.IsMouseClicked ImGuiMouseButton.Left then
                let mousePosition = World.getMousePosition world
                match tryMousePick mousePosition with
                | Some (_, entity) ->
                    if entity.GetIs2d world then
                        snapshot ()
                        if World.isKeyboardAltDown world then
                            let viewport = World.getViewport world
                            let eyeCenter = World.getEyeCenter2d world
                            let eyeSize = World.getEyeSize2d world
                            let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                            let entityDegrees = if entity.MountExists world then entity.GetDegreesLocal world else entity.GetDegrees world
                            dragEntityState <- DragEntityRotation2d (DateTimeOffset.Now, mousePositionWorld, entityDegrees.Z + mousePositionWorld.Y, entity)
                        else
                            let viewport = World.getViewport world
                            let eyeCenter = World.getEyeCenter2d world
                            let eyeSize = World.getEyeSize2d world
                            let mousePositionWorld = viewport.MouseToWorld2d (entity.GetAbsolute world, mousePosition, eyeCenter, eyeSize)
                            let entityPosition = entity.GetPosition world
                            dragEntityState <- DragEntityPosition2d (DateTimeOffset.Now, mousePositionWorld, entityPosition.V2 + mousePositionWorld, entity)
                | None -> ()

            match dragEntityState with
            | DragEntityPosition2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Gaia.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityPosition = (entityDragOffset - mousePositionWorldOriginal) + (mousePositionWorld - mousePositionWorldOriginal)
                    let entityPositionSnapped =
                        if snaps2dSelected && ImGui.IsCtrlReleased ()
                        then Math.SnapF3d (Triple.fst (getSnaps ())) entityPosition.V3
                        else entityPosition.V3
                    let entityPosition = entity.GetPosition world
                    let entityPositionDelta = entityPositionSnapped - entityPosition
                    let entityPositionConstrained = entityPosition + entityPositionDelta
                    match Option.bind (tryResolve entity) (entity.GetMountOpt world) with
                    | Some parent ->
                        let entityPositionLocal = Vector3.Transform (entityPositionConstrained, (parent.GetAffineMatrix world).Inverted)
                        world <- entity.SetPositionLocal entityPositionLocal world
                    | None ->
                        world <- entity.SetPosition entityPositionConstrained world
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                        world <- entity.SetLinearVelocity v3Zero world
                        world <- entity.SetAngularVelocity v3Zero world
            | DragEntityRotation2d (time, mousePositionWorldOriginal, entityDragOffset, entity) ->
                let localTime = DateTimeOffset.Now - time
                if entity.Exists world && localTime.TotalSeconds >= Constants.Gaia.DragMinimumSeconds then
                    let mousePositionWorld = World.getMousePostion2dWorld (entity.GetAbsolute world) world
                    let entityDegree = (entityDragOffset - mousePositionWorldOriginal.Y) + (mousePositionWorld.Y - mousePositionWorldOriginal.Y)
                    let entityDegreeSnapped =
                        if snaps2dSelected && ImGui.IsCtrlReleased ()
                        then Math.SnapF (Triple.snd (getSnaps ())) entityDegree
                        else entityDegree
                    let entityDegree = (entity.GetDegreesLocal world).Z
                    if entity.MountExists world then
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegreeLocal = entityDegree + entityDegreeDelta
                        world <- entity.SetDegreesLocal (v3 0.0f 0.0f entityDegreeLocal) world
                    else
                        let entityDegreeDelta = entityDegreeSnapped - entityDegree
                        let entityDegree = entityDegree + entityDegreeDelta
                        world <- entity.SetDegrees (v3 0.0f 0.0f entityDegree) world
                    if  Option.isSome (entity.TryGetProperty "LinearVelocity" world) &&
                        Option.isSome (entity.TryGetProperty "AngularVelocity" world) then
                        world <- entity.SetLinearVelocity v3Zero world
                        world <- entity.SetAngularVelocity v3Zero world
            | DragEntityInactive -> ()

        if ImGui.IsMouseReleased ImGuiMouseButton.Left then
            match dragEntityState with
            | DragEntityPosition2d _ | DragEntityRotation2d _ -> dragEntityState <- DragEntityInactive
            | DragEntityInactive -> ()

    let private updateEyeDrag () =

        if canEditWithMouse () then

            if ImGui.IsMouseClicked ImGuiMouseButton.Middle then
                let mousePositionScreen = World.getMousePosition2dScreen world
                let dragState = DragEyeCenter2d (World.getEyeCenter2d world + mousePositionScreen, mousePositionScreen)
                dragEyeState <- dragState

            match dragEyeState with
            | DragEyeCenter2d (entityDragOffset, mousePositionScreenOrig) ->
                let mousePositionScreen = World.getMousePosition2dScreen world
                desiredEyeCenter2d <- (entityDragOffset - mousePositionScreenOrig) + -Constants.Gaia.EyeSpeed * (mousePositionScreen - mousePositionScreenOrig)
                dragEyeState <- DragEyeCenter2d (entityDragOffset, mousePositionScreenOrig)
            | DragEyeInactive -> ()

        if ImGui.IsMouseReleased ImGuiMouseButton.Middle then
            match dragEyeState with
            | DragEyeCenter2d _ -> dragEyeState <- DragEyeInactive
            | DragEyeInactive -> ()

    let private updateEyeTravel () =
        if canEditWithKeyboard () then
            let position = World.getEyeCenter3d world
            let rotation = World.getEyeRotation3d world
            let moveSpeed =
                if ImGui.IsKeyDown ImGuiKey.Enter && ImGui.IsShiftDown () then 5.0f
                elif ImGui.IsKeyDown ImGuiKey.Enter then 0.5f
                elif ImGui.IsShiftDown () then 0.02f
                else 0.12f
            let turnSpeed =
                if ImGui.IsShiftDown () && not (ImGui.IsKeyDown ImGuiKey.Enter) then 0.025f
                else 0.05f
            if ImGui.IsKeyDown ImGuiKey.W && ImGui.IsCtrlReleased () then
                desiredEyeCenter3d <- position + Vector3.Transform (v3Forward, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.S && ImGui.IsCtrlReleased () then
                desiredEyeCenter3d <- position + Vector3.Transform (v3Back, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.A && ImGui.IsCtrlReleased () then
                desiredEyeCenter3d <- position + Vector3.Transform (v3Left, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.D && ImGui.IsCtrlReleased () then
                desiredEyeCenter3d <- position + Vector3.Transform (v3Right, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.Q && ImGui.IsCtrlReleased () then
                let rotation' = rotation * Quaternion.CreateFromAxisAngle (v3Right, turnSpeed)
                if Vector3.Dot (rotation'.Forward, v3Up) < 0.999f then desiredEyeRotation3d <- rotation'
            if ImGui.IsKeyDown ImGuiKey.E && ImGui.IsCtrlReleased () then
                let rotation' = rotation * Quaternion.CreateFromAxisAngle (v3Left, turnSpeed)
                if Vector3.Dot (rotation'.Forward, v3Down) < 0.999f then desiredEyeRotation3d <- rotation'
            if ImGui.IsKeyDown ImGuiKey.UpArrow && ImGui.IsAltReleased () then
                desiredEyeCenter3d <- position + Vector3.Transform (v3Up, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.DownArrow && ImGui.IsAltReleased () then
                desiredEyeCenter3d <- position + Vector3.Transform (v3Down, rotation) * moveSpeed
            if ImGui.IsKeyDown ImGuiKey.LeftArrow && ImGui.IsAltReleased () then
                desiredEyeRotation3d <- Quaternion.CreateFromAxisAngle (v3Up, turnSpeed) * rotation
            if ImGui.IsKeyDown ImGuiKey.RightArrow && ImGui.IsAltReleased () then
                desiredEyeRotation3d <- Quaternion.CreateFromAxisAngle (v3Down, turnSpeed) * rotation

    let private updateHotkeys entityHierarchyFocused =
        if not (modal ()) then
            if ImGui.IsKeyPressed ImGuiKey.F2 && selectedEntityOpt.IsSome && not (selectedEntityOpt.Value.GetProtected world) then showRenameEntityDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F4 && ImGui.IsAltDown () then showConfirmExitDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.F5 then toggleAdvancing ()
            elif ImGui.IsKeyPressed ImGuiKey.F6 then editWhileAdvancing <- not editWhileAdvancing
            elif ImGui.IsKeyPressed ImGuiKey.F8 then reloadAssetsRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F9 then reloadCodeRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.F11 then fullScreen <- not fullScreen
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () && ImGui.IsShiftDown () then showOpenProjectDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.N && ImGui.IsCtrlDown () then showNewGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.O && ImGui.IsCtrlDown () then showOpenGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.S && ImGui.IsCtrlDown () then showSaveGroupDialog <- true
            elif ImGui.IsKeyPressed ImGuiKey.Q && ImGui.IsCtrlDown () then tryQuickSizeSelectedEntity () |> ignore<bool>
            elif ImGui.IsKeyPressed ImGuiKey.R && ImGui.IsCtrlDown () then reloadAllRequested <- 1
            elif ImGui.IsKeyPressed ImGuiKey.UpArrow && ImGui.IsAltDown () then tryReorderSelectedEntity true
            elif ImGui.IsKeyPressed ImGuiKey.DownArrow && ImGui.IsAltDown () then tryReorderSelectedEntity false
            elif not (ImGui.GetIO ()).WantCaptureKeyboardPlus || entityHierarchyFocused then
                if ImGui.IsKeyPressed ImGuiKey.Z && ImGui.IsCtrlDown () then tryUndo () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Y && ImGui.IsCtrlDown () then tryRedo () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.X && ImGui.IsCtrlDown () then tryCutSelectedEntity () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.C && ImGui.IsCtrlDown () then tryCopySelectedEntity () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.V && ImGui.IsCtrlDown () then tryPaste false |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Enter && ImGui.IsCtrlDown () then createEntity false false
                elif ImGui.IsKeyPressed ImGuiKey.Delete then tryDeleteSelectedEntity () |> ignore<bool>
                elif ImGui.IsKeyPressed ImGuiKey.Escape then focusedPropertyDescriptorOpt <- None; selectEntityOpt None

    let rec private imGuiEntityHierarchy (entity : Entity) =
        let children = world |> entity.GetChildren |> Seq.toArray
        let selected = match selectedEntityOpt with Some selectedEntity -> entity = selectedEntity | None -> false
        let treeNodeFlags =
            (if selected then ImGuiTreeNodeFlags.Selected else ImGuiTreeNodeFlags.None) |||
            (if Array.isEmpty children then ImGuiTreeNodeFlags.Leaf else ImGuiTreeNodeFlags.None) |||
            ImGuiTreeNodeFlags.SpanAvailWidth ||| ImGuiTreeNodeFlags.OpenOnArrow
        if expandEntityHierarchy then ImGui.SetNextItemOpen true
        if collapseEntityHierarchy then ImGui.SetNextItemOpen false
        match selectedEntityOpt with
        | Some selectedEntity when showSelectedEntity ->
            let relation = relate entity selectedEntity
            if  Array.notExists (fun t -> t = Token.Parent || t = Token.Current) relation.Tokens &&
                relation.Tokens.Length > 0 then
                ImGui.SetNextItemOpen true
        | Some _ | None -> ()
        let expanded = ImGui.TreeNodeEx (entity.Name, treeNodeFlags)
        if showSelectedEntity && Some entity = selectedEntityOpt then
            ImGui.SetScrollHereY ()
            showSelectedEntity <- false
        if ImGui.IsKeyPressed ImGuiKey.Space && ImGui.IsItemFocused () && ImGui.IsWindowFocused () then
            selectEntityOpt (Some entity)
        if ImGui.IsMouseClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            selectEntityOpt (Some entity)
        if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
            if not (entity.GetAbsolute world) then
                if entity.GetIs2d world then
                    desiredEyeCenter2d <- (entity.GetCenter world).V2
                else
                    let eyeRotation = World.getEyeRotation3d world
                    let eyeCenterOffset = Vector3.Transform (Constants.Engine.EyeCenter3dOffset, eyeRotation)
                    desiredEyeCenter3d <- entity.GetPosition world + eyeCenterOffset
        if ImGui.BeginPopupContextItem () then
            selectEntityOpt (Some entity)
            if ImGui.MenuItem "Cut" then tryCutSelectedEntity () |> ignore<bool>
            if ImGui.MenuItem "Copy" then tryCopySelectedEntity () |> ignore<bool>
            ImGui.Separator ()
            if ImGui.MenuItem "Delete" then tryDeleteSelectedEntity () |> ignore<bool>
            ImGui.Separator ()
            match selectedEntityOpt with
            | Some entity when entity.Exists world ->
                if entity.GetStatic world
                then if ImGui.MenuItem "Make Entity Family Non-Static" then trySetSelectedEntityFamilyStatic false
                else if ImGui.MenuItem "Make Entity Family Static" then trySetSelectedEntityFamilyStatic true
            | Some _ | None -> ()
            ImGui.EndPopup ()
        if ImGui.BeginDragDropSource () then
            let entityAddressStr = entity.EntityAddress |> scstring |> Symbol.distill
            dragDropPayloadOpt <- Some entityAddressStr
            ImGui.Text entity.Name
            ImGui.SetDragDropPayload ("Entity", IntPtr.Zero, 0u) |> ignore<bool>
            ImGui.EndDragDropSource ()
        if ImGui.BeginDragDropTarget () then
            if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr) then
                match dragDropPayloadOpt with
                | Some payload ->
                    let sourceEntityAddressStr = payload
                    let sourceEntity = Nu.Entity sourceEntityAddressStr
                    if not (sourceEntity.GetProtected world) then
                        if ImGui.IsAltDown () then
                            let next = Nu.Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames)
                            let previousOpt = World.tryGetPreviousEntity next world
                            let parentOpt = match next.Parent with :? Entity as parent -> Some parent | _ -> None
                            let mountOpt = match parentOpt with Some _ -> Some (Relation.makeParent ()) | None -> None
                            let sourceEntity' = match parentOpt with Some parent -> parent / sourceEntity.Name | None -> selectedGroup / sourceEntity.Name
                            world <- World.insertEntityOrder sourceEntity previousOpt next world
                            world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                            world <- sourceEntity'.SetMountOptWithAdjustment mountOpt world
                            selectEntityOpt (Some sourceEntity')
                            showSelectedEntity <- true
                        else
                            let sourceEntity' = Nu.Entity (selectedGroup.GroupAddress <-- Address.makeFromArray entity.Surnames) / sourceEntity.Name
                            let mount = Relation.makeParent ()
                            world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                            world <- sourceEntity'.SetMountOptWithAdjustment (Some mount) world
                            selectEntityOpt (Some sourceEntity')
                            showSelectedEntity <- true
                    else messageBoxOpt <- Some "Cannot relocate a protected simulant (such as an entity created by the MMCC API)."
                | None -> ()
        if expanded then
            for child in children do imGuiEntityHierarchy child
            ImGui.TreePop ()

    let private imGuiEditMaterialProperiesProperty mp propertyDescriptor simulant =

        // edit albedo
        let mutable isSome = ValueOption.isSome mp.AlbedoOpt
        if ImGui.Checkbox ((if isSome then "##mpAlbedoIsSome" else "AlbedoOpt"), &isSome) then
            if isSome
            then setPropertyValue { mp with AlbedoOpt = ValueSome Constants.Render.AlbedoDefault } propertyDescriptor simulant
            else setPropertyValue { mp with AlbedoOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.AlbedoOpt with
            | ValueSome albedo ->
                let mutable v = v4 albedo.R albedo.G albedo.B albedo.A
                ImGui.SameLine ()
                if ImGui.ColorEdit4 ("AlbedoOpt", &v) then setPropertyValue { mp with AlbedoOpt = ValueSome (color v.X v.Y v.Z v.W) } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)

        // edit roughness
        let mutable isSome = ValueOption.isSome mp.RoughnessOpt
        if ImGui.Checkbox ((if isSome then "##mpRoughnessIsSome" else "RoughnessOpt"), &isSome) then
            if isSome
            then setPropertyValue { mp with RoughnessOpt = ValueSome Constants.Render.RoughnessDefault } propertyDescriptor simulant
            else setPropertyValue { mp with RoughnessOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.RoughnessOpt with
            | ValueSome roughness ->
                let mutable roughness = roughness
                ImGui.SameLine ()
                if ImGui.SliderFloat ("RoughnessOpt", &roughness, 0.0f, 1.0f) then setPropertyValue { mp with RoughnessOpt = ValueSome roughness } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)

        // edit metallic
        let mutable isSome = ValueOption.isSome mp.MetallicOpt
        if ImGui.Checkbox ((if isSome then "##mpMetallicIsSome" else "MetallicOpt"), &isSome) then
            if isSome
            then setPropertyValue { mp with MetallicOpt = ValueSome Constants.Render.MetallicDefault } propertyDescriptor simulant
            else setPropertyValue { mp with MetallicOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.MetallicOpt with
            | ValueSome metallic ->
                let mutable metallic = metallic
                ImGui.SameLine ()
                if ImGui.SliderFloat ("MetallicOpt", &metallic, 0.0f, 1.0f) then setPropertyValue { mp with MetallicOpt = ValueSome metallic } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)

        // edit ambient occlusion
        let mutable isSome = ValueOption.isSome mp.AmbientOcclusionOpt
        if ImGui.Checkbox ((if isSome then "##mpAmbientOcclusionIsSome" else "AmbientOcclusionOpt"), &isSome) then
            if isSome
            then setPropertyValue { mp with AmbientOcclusionOpt = ValueSome Constants.Render.AmbientOcclusionDefault } propertyDescriptor simulant
            else setPropertyValue { mp with AmbientOcclusionOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.AmbientOcclusionOpt with
            | ValueSome ambientOcclusion ->
                let mutable ambientOcclusion = ambientOcclusion
                ImGui.SameLine ()
                if ImGui.SliderFloat ("AmbientOcclusionOpt", &ambientOcclusion, 0.0f, 10.0f) then setPropertyValue { mp with AmbientOcclusionOpt = ValueSome ambientOcclusion } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)

        // edit emission
        let mutable isSome = ValueOption.isSome mp.EmissionOpt
        if ImGui.Checkbox ((if isSome then "##mpEmissionIsSome" else "EmissionOpt"), &isSome) then
            if isSome
            then setPropertyValue { mp with EmissionOpt = ValueSome Constants.Render.EmissionDefault } propertyDescriptor simulant
            else setPropertyValue { mp with EmissionOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.EmissionOpt with
            | ValueSome emission ->
                let mutable emission = emission
                ImGui.SameLine ()
                if ImGui.SliderFloat ("EmissionOpt", &emission, 0.0f, 10.0f) then setPropertyValue { mp with EmissionOpt = ValueSome emission } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)

        // edit height
        let mutable isSome = ValueOption.isSome mp.HeightOpt
        if ImGui.Checkbox ((if isSome then "##mpHeightIsSome" else "HeightOpt"), &isSome) then
            if isSome
            then setPropertyValue { mp with HeightOpt = ValueSome Constants.Render.HeightDefault } propertyDescriptor simulant
            else setPropertyValue { mp with HeightOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.HeightOpt with
            | ValueSome height ->
                let mutable height = height
                ImGui.SameLine ()
                if ImGui.SliderFloat ("HeightOpt", &height, 0.0f, 10.0f) then setPropertyValue { mp with HeightOpt = ValueSome height } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)

        // edit invert roughness
        let mutable isSome = ValueOption.isSome mp.InvertRoughnessOpt
        if ImGui.Checkbox ((if isSome then "##mpInvertRoughnessIsSome" else "InvertRoughnessOpt"), &isSome) then
            if isSome
            then setPropertyValue { mp with InvertRoughnessOpt = ValueSome Constants.Render.InvertRoughnessDefault } propertyDescriptor simulant
            else setPropertyValue { mp with InvertRoughnessOpt = ValueNone } propertyDescriptor simulant
        else
            match mp.InvertRoughnessOpt with
            | ValueSome invertRoughness ->
                let mutable invertRoughness = invertRoughness
                ImGui.SameLine ()
                if ImGui.Checkbox ("InvertRoughnessOpt", &invertRoughness) then setPropertyValue { mp with InvertRoughnessOpt = ValueSome invertRoughness } propertyDescriptor simulant
                if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
            | ValueNone -> ()
        if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)

    let rec private imGuiEditProperty (getProperty : PropertyDescriptor -> Simulant -> obj) (setProperty : obj -> PropertyDescriptor -> Simulant -> unit) (focusProperty : unit -> unit) (propertyLabelPrefix : string) (propertyDescriptor : PropertyDescriptor) (simulant : Simulant) =
        let ty = propertyDescriptor.PropertyType
        let name = propertyDescriptor.PropertyName
        let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType, toSymbolMemo, ofSymbolMemo)
        let propertyValue = getProperty propertyDescriptor simulant
        let propertyValueStr = converter.ConvertToString propertyValue
        match propertyValue with
        | :? Frustum -> () // TODO: implement FrustumConverter.
        | :? bool as b -> let mutable b' = b in if ImGui.Checkbox (name, &b') then setProperty b' propertyDescriptor simulant
        | :? int8 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (int8 i') propertyDescriptor simulant
        | :? uint8 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (uint8 i') propertyDescriptor simulant
        | :? int16 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (int16 i') propertyDescriptor simulant
        | :? uint16 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (uint16 i') propertyDescriptor simulant
        | :? int32 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (int32 i') propertyDescriptor simulant
        | :? uint32 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (uint32 i') propertyDescriptor simulant
        | :? int64 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (int64 i') propertyDescriptor simulant
        | :? uint64 as i -> let mutable i' = int32 i in if ImGui.DragInt (name, &i') then setProperty (uint64 i') propertyDescriptor simulant
        | :? single as f -> let mutable f' = single f in if ImGui.DragFloat (name, &f', snapDrag) then setProperty (single f') propertyDescriptor simulant
        | :? double as f -> let mutable f' = single f in if ImGui.DragFloat (name, &f', snapDrag) then setProperty (double f') propertyDescriptor simulant
        | :? Vector2 as v -> let mutable v' = v in if ImGui.DragFloat2 (name, &v', snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector3 as v -> let mutable v' = v in if ImGui.DragFloat3 (name, &v', snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector4 as v -> let mutable v' = v in if ImGui.DragFloat4 (name, &v', snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector2i as v -> let mutable v' = v in if ImGui.DragInt2 (name, &v'.X, snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector3i as v -> let mutable v' = v in if ImGui.DragInt3 (name, &v'.X, snapDrag) then setProperty v' propertyDescriptor simulant
        | :? Vector4i as v -> let mutable v' = v in if ImGui.DragInt4 (name, &v'.X, snapDrag) then setProperty v' propertyDescriptor simulant
        | :? MaterialProperties as mp -> imGuiEditMaterialProperiesProperty mp propertyDescriptor simulant
        | :? Box2 as b ->
            ImGui.Text name
            let mutable min = v2 b.Min.X b.Min.Y
            let mutable size = v2 b.Size.X b.Size.Y
            ImGui.Indent ()
            let minChanged = ImGui.DragFloat2 (propertyLabelPrefix + "Min via " + name, &min, snapDrag)
            focusProperty ()
            let sizeChanged = ImGui.DragFloat2 (propertyLabelPrefix + "Size via " + name, &size, snapDrag)
            if minChanged || sizeChanged then
                let b' = box2 min size
                setProperty b' propertyDescriptor simulant
            ImGui.Unindent ()
        | :? Box3 as b ->
            ImGui.Text name
            let mutable min = v3 b.Min.X b.Min.Y b.Min.Z
            let mutable size = v3 b.Size.X b.Size.Y b.Size.Z
            ImGui.Indent ()
            let minChanged = ImGui.DragFloat3 (propertyLabelPrefix + "Min via " + name, &min, snapDrag)
            focusProperty ()
            let sizeChanged = ImGui.DragFloat3 (propertyLabelPrefix + "Size via " + name, &size, snapDrag)
            if minChanged || sizeChanged then
                let b' = box3 min size
                setProperty b' propertyDescriptor simulant
            ImGui.Unindent ()
        | :? Box2i as b ->
            ImGui.Text name
            let mutable min = v2i b.Min.X b.Min.Y
            let mutable size = v2i b.Size.X b.Size.Y
            ImGui.Indent ()
            let minChanged = ImGui.DragInt2 (propertyLabelPrefix + "Min via " + name, &min.X, snapDrag)
            focusProperty ()
            let sizeChanged = ImGui.DragInt2 (propertyLabelPrefix + "Size via " + name, &size.X, snapDrag)
            if minChanged || sizeChanged then
                let b' = box2i min size
                setProperty b' propertyDescriptor simulant
            ImGui.Unindent ()
        | :? Quaternion as q ->
            let mutable v = v4 q.X q.Y q.Z q.W
            if ImGui.DragFloat4 (name, &v, snapDrag) then
                let q' = quat v.X v.Y v.Z v.W
                setProperty q' propertyDescriptor simulant
        | :? Color as c ->
            let mutable v = v4 c.R c.G c.B c.A
            if ImGui.ColorEdit4 (name, &v) then
                let c' = color v.X v.Y v.Z v.W
                setPropertyValue c' propertyDescriptor simulant
        | :? RenderStyle as style ->
            let mutable index = match style with Deferred -> 0 | Forward _ -> 1
            let (changed, style) =
                if ImGui.Combo (name, &index, [|nameof Deferred; nameof Forward|], 2)
                then (true, match index with 0 -> Deferred | 1 -> Forward (0.0f, 0.0f) | _ -> failwithumf ())
                else (false, style)
            focusProperty ()
            let (changed, style) =
                match index with
                | 0 -> (changed, style)
                | 1 ->
                    match style with
                    | Deferred -> failwithumf ()
                    | Forward (subsort, sort) ->
                        let mutable (subsort, sort) = (subsort, sort)
                        ImGui.Indent ()
                        let subsortChanged = ImGui.DragFloat ("Subsort via " + name, &subsort, snapDrag)
                        focusProperty ()
                        let sortChanged = ImGui.DragFloat ("Sort via " + name, &sort, snapDrag)
                        ImGui.Unindent ()
                        (changed || subsortChanged || sortChanged, Forward (subsort, sort))
                | _ -> failwithumf ()
            if changed then setProperty style propertyDescriptor simulant
        | :? LightType as light ->
            let mutable index = match light with PointLight -> 0 | DirectionalLight -> 1 | SpotLight _ -> 2
            let (changed, light) =
                if ImGui.Combo (name, &index, [|nameof PointLight; nameof DirectionalLight; nameof SpotLight|], 3)
                then (true, match index with 0 -> PointLight | 1 -> DirectionalLight | 2 -> SpotLight (0.9f, 1.0f) | _ -> failwithumf ())
                else (false, light)
            focusProperty ()
            let (changed, light) =
                match index with
                | 0 -> (changed, light)
                | 1 -> (changed, light)
                | 2 ->
                    match light with
                    | PointLight -> failwithumf ()
                    | DirectionalLight -> failwithumf ()
                    | SpotLight (innerCone, outerCone) ->
                        let mutable (innerCone, outerCone) = (innerCone, outerCone)
                        ImGui.Indent ()
                        let innerConeChanged = ImGui.DragFloat ("InnerCone via " + name, &innerCone, snapDrag)
                        focusProperty ()
                        let outerConeChanged = ImGui.DragFloat ("OuterCone via " + name, &outerCone, snapDrag)
                        ImGui.Unindent ()
                        (changed || innerConeChanged || outerConeChanged, SpotLight (innerCone, outerCone))
                | _ -> failwithumf ()
            if changed then setProperty light propertyDescriptor simulant
        | :? Substance as substance ->
            let mutable scalar = match substance with Mass m -> m | Density d -> d
            let changed = ImGui.DragFloat ("##scalar via " + name, &scalar, snapDrag)
            focusProperty ()
            let mutable index = match substance with Mass _ -> 0 | Density _ -> 1
            ImGui.SameLine ()
            let changed = ImGui.Combo (name, &index, [|nameof Mass; nameof Density|], 2) || changed
            if changed then
                let substance = match index with 0 -> Mass scalar | 1 -> Density scalar | _ -> failwithumf ()
                setProperty substance propertyDescriptor simulant
        | _ ->
            let mutable combo = false
            if FSharpType.IsUnion ty then
                let cases = FSharpType.GetUnionCases ty
                if Array.forall (fun (case : UnionCaseInfo) -> Array.isEmpty (case.GetFields ())) cases then
                    combo <- true
                    let caseNames = Array.map (fun (case : UnionCaseInfo) -> case.Name) cases
                    let (unionCaseInfo, _) = FSharpValue.GetUnionFields (propertyValue, ty)
                    let mutable tag = unionCaseInfo.Tag
                    if ImGui.Combo (name, &tag, caseNames, caseNames.Length) then
                        let value' = FSharpValue.MakeUnion (cases.[tag], [||])
                        setProperty value' propertyDescriptor simulant
            if not combo then
                if ty.IsGenericType &&
                   ty.GetGenericTypeDefinition () = typedefof<_ option> &&
                   ty.GenericTypeArguments.[0] <> typedefof<_ option> &&
                   ty.GenericTypeArguments.[0] <> typeof<MaterialProperties> &&
                   (ty.GenericTypeArguments.[0].IsValueType && ty.GenericTypeArguments.[0].Name <> typedefof<_ AssetTag>.Name ||
                    ty.GenericTypeArguments.[0] = typeof<string> ||
                    ty.GenericTypeArguments.[0] |> FSharpType.isNullTrueValue) then
                    let mutable isSome = ty.GetProperty("IsSome").GetValue(null, [|propertyValue|]) :?> bool
                    if ImGui.Checkbox ((if isSome then "##" else "") + name, &isSome) then
                        if isSome then
                            if ty.GenericTypeArguments.[0].IsValueType then
                                if ty.GenericTypeArguments.[0].Name = typedefof<_ AssetTag>.Name
                                then setProperty (Activator.CreateInstance (ty, [|Activator.CreateInstance (ty.GenericTypeArguments.[0], [|""; ""|])|])) propertyDescriptor simulant
                                else setProperty (Activator.CreateInstance (ty, [|Activator.CreateInstance ty.GenericTypeArguments.[0]|])) propertyDescriptor simulant
                            elif ty.GenericTypeArguments.[0] = typeof<string> then
                                setProperty (Activator.CreateInstance (ty, [|""|])) propertyDescriptor simulant
                            elif FSharpType.isNullTrueValue ty.GenericTypeArguments.[0] then
                                setProperty (Activator.CreateInstance (ty, [|null|])) propertyDescriptor simulant
                            else
                                () // TODO: look up default values from overlayer if they are some
                        else setProperty None propertyDescriptor simulant
                    focusProperty ()
                    if isSome then
                        ImGui.SameLine ()
                        let getProperty = fun _ simulant -> let opt = getProperty propertyDescriptor simulant in ty.GetProperty("Value").GetValue(opt, [||])
                        let setProperty = fun value _ simulant -> setProperty (Activator.CreateInstance (ty, [|value|])) propertyDescriptor simulant
                        let propertyDescriptor = { propertyDescriptor with PropertyType = ty.GenericTypeArguments.[0] }
                        imGuiEditProperty getProperty setProperty focusProperty (name + ".") propertyDescriptor simulant
                elif ty.IsGenericType &&
                     ty.GetGenericTypeDefinition () = typedefof<_ voption> &&
                     ty.GenericTypeArguments.[0] <> typedefof<_ voption> &&
                     ty.GenericTypeArguments.[0] <> typeof<MaterialProperties> &&
                     (ty.GenericTypeArguments.[0].IsValueType && ty.GenericTypeArguments.[0].Name <> typedefof<_ AssetTag>.Name ||
                      ty.GenericTypeArguments.[0] = typeof<string> ||
                      ty.GenericTypeArguments.[0] |> FSharpType.isNullTrueValue) then
                    let mutable isSome = ty.GetProperty("IsSome").GetValue(null, [|propertyValue|]) :?> bool
                    if ImGui.Checkbox ((if isSome then "##" else "") + name, &isSome) then
                        if isSome then
                            if ty.GenericTypeArguments.[0].IsValueType then
                                setProperty (Activator.CreateInstance (ty, [|Activator.CreateInstance ty.GenericTypeArguments.[0]|])) propertyDescriptor simulant
                            elif ty.GenericTypeArguments.[0] = typeof<string> then
                                setProperty (Activator.CreateInstance (ty, [|""|])) propertyDescriptor simulant
                            elif FSharpType.isNullTrueValue ty.GenericTypeArguments.[0] then
                                setProperty (Activator.CreateInstance (ty, [|null|])) propertyDescriptor simulant
                            else
                                failwithumf ()
                        else setProperty ValueNone propertyDescriptor simulant
                    focusProperty ()
                    if isSome then
                        ImGui.SameLine ()
                        let getProperty = fun _ simulant -> let opt = getProperty propertyDescriptor simulant in ty.GetProperty("Value").GetValue(opt, [||])
                        let setProperty = fun value _ simulant -> setProperty (Activator.CreateInstance (ty, [|value|])) propertyDescriptor simulant
                        let propertyDescriptor = { propertyDescriptor with PropertyType = ty.GenericTypeArguments.[0] }
                        imGuiEditProperty getProperty setProperty focusProperty (name + ".") propertyDescriptor simulant
                elif ty.IsGenericType && ty.GetGenericTypeDefinition () = typedefof<_ AssetTag> then
                    let mutable propertyValueStr = propertyValueStr
                    if ImGui.InputText ("##text" + name, &propertyValueStr, 4096u) then
                        let worldsPast' = worldsPast
                        try let propertyValue = converter.ConvertFromString propertyValueStr
                            setProperty propertyValue propertyDescriptor simulant
                        with _ ->
                            worldsPast <- worldsPast'
                    focusProperty ()
                    if ImGui.BeginDragDropTarget () then
                        if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                            match dragDropPayloadOpt with
                            | Some payload ->
                                let worldsPast' = worldsPast
                                try let propertyValueEscaped = payload
                                    let propertyValueUnescaped = String.unescape propertyValueEscaped
                                    let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                    setProperty propertyValue propertyDescriptor simulant
                                with _ ->
                                    worldsPast <- worldsPast'
                            | None -> ()
                        ImGui.EndDragDropTarget ()
                    ImGui.SameLine ()
                    ImGui.PushID ("##pickAsset" + name)
                    if ImGui.Button ("V", v2Dup 19.0f) then showAssetPickerDialog <- true
                    focusProperty ()
                    ImGui.PopID ()
                    ImGui.SameLine ()
                    ImGui.Text name
                elif name = Constants.Engine.FacetNamesPropertyName && ty = typeof<string Set> then
                    let facetNamesAll = world |> World.getFacets |> Map.toKeyArray |> Array.append [|"(Empty)"|]
                    let facetNames = scvalue<string Set> propertyValueStr
                    let mutable facetNames' = Set.empty
                    let mutable changed = false
                    ImGui.Indent ()
                    for i in 0 .. facetNames.Count do
                        let last = i = facetNames.Count
                        let facetName = if not last then Seq.item i facetNames else "(Empty)"
                        let mutable facetNameIndex = Seq.findIndex ((=) facetName) facetNamesAll
                        changed <- ImGui.Combo ("##" + name + string i, &facetNameIndex, facetNamesAll, facetNamesAll.Length) || changed
                        if not last then focusProperty ()
                        if facetNameIndex <> 0 then facetNames' <- Set.add (Seq.item facetNameIndex facetNamesAll) facetNames'
                    ImGui.Unindent ()
                    if changed then setPropertyValueIgnoreError facetNames' propertyDescriptor simulant
                else
                    let mutable propertyValueStr = propertyValueStr
                    if ImGui.InputText (name, &propertyValueStr, 131072u) && propertyValueStr <> propertyValueStrPrevious then
                        let worldsPast' = worldsPast
                        try let propertyValue = converter.ConvertFromString propertyValueStr
                            setProperty propertyValue propertyDescriptor simulant
                        with _ ->
                            worldsPast <- worldsPast'
                        propertyValueStrPrevious <- propertyValueStr
        focusProperty ()

    let private imGuiEditProperties (simulant : Simulant) =
        let mutable simulant = simulant
        let propertyDescriptors = world |> SimulantPropertyDescriptor.getPropertyDescriptors simulant |> Array.ofList
        let propertyDescriptorses = propertyDescriptors |> Array.groupBy EntityPropertyDescriptor.getCategory |> Map.ofSeq
        for (propertyCategory, propertyDescriptors) in propertyDescriptorses.Pairs do
            if ImGui.CollapsingHeader (propertyCategory, ImGuiTreeNodeFlags.DefaultOpen ||| ImGuiTreeNodeFlags.OpenOnArrow) then
                let propertyDescriptors =
                    propertyDescriptors |>
                    Array.filter (fun pd -> SimulantPropertyDescriptor.getEditable pd simulant) |>
                    Array.sortBy (fun pd ->
                        match pd.PropertyName with
                        | Constants.Engine.NamePropertyName -> "!" // put Name first
                        | Constants.Engine.ModelPropertyName -> "!2" // put Model second
                        | Constants.Engine.MountOptPropertyName -> "!3" // put MountOpt third
                        | Constants.Engine.OverlayNameOptPropertyName -> "!4" // put OverlayNameOpt fourth
                        | name -> name)
                for propertyDescriptor in propertyDescriptors do
                    if containsProperty propertyDescriptor simulant then
                        if propertyDescriptor.PropertyName = Constants.Engine.NamePropertyName then // NOTE: name edit properties can't be replaced.
                            match simulant with
                            | :? Screen as screen ->
                                // NOTE: can't edit screen names for now.
                                let mutable name = screen.Name
                                ImGui.InputText ("Name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                            | :? Group as group ->
                                let mutable name = group.Name
                                ImGui.InputText ("##name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                if not (group.GetProtected world) then
                                    if ImGui.Button "Rename" then
                                        showRenameGroupDialog <- true
                                else ImGui.Text "Name"
                            | :? Entity as entity ->
                                let mutable name = entity.Name
                                ImGui.InputText ("##name", &name, 4096u, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                                ImGui.SameLine ()
                                if not (entity.GetProtected world) then
                                    if ImGui.Button "Rename" then
                                        showRenameEntityDialog <- true
                                else ImGui.Text "Name"
                            | _ -> ()
                            if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- None
                        elif propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                            let mutable clickToEditModel = "*click to edit*"
                            ImGui.InputText ("Model", &clickToEditModel, 256u) |> ignore<bool>
                            if ImGui.IsItemFocused () then
                                focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                                focusPropertyEditorRequested <- true
                        else
                            let focusProperty = fun () -> if ImGui.IsItemFocused () then focusedPropertyDescriptorOpt <- Some (propertyDescriptor, simulant)
                            let mutable replaced = false
                            let replaceProperty =
                                ReplaceProperty
                                    { Snapshot = fun world -> snapshot (); world
                                      FocusProperty = fun world -> focusProperty (); world
                                      IndicateReplaced = fun world -> replaced <- true; world
                                      PropertyDescriptor = propertyDescriptor }
                            world <- World.edit replaceProperty simulant world
                            if not replaced then
                                imGuiEditProperty getPropertyValue setPropertyValue focusProperty "" propertyDescriptor simulant
        let appendProperties =
            { Snapshot = fun world -> snapshot (); world
              UnfocusProperty = fun world -> focusedPropertyDescriptorOpt <- None; world }
        world <- World.edit (AppendProperties appendProperties) simulant world

    let private imGuiProcess wtemp =

        // store old world
        let worldOld = wtemp

        // transfer to world mutation mode
        world <- worldOld

        // see if sync eyes to editor is desirable
        let eyeCenter2d = World.getEyeCenter2d world
        let eyeCenter3d = World.getEyeCenter3d world
        let eyeRotation3d = World.getEyeRotation3d world
        let eyeChangedElsewhere =
            eyeCenter2d <> desiredEyeCenter2d ||
            eyeCenter3d <> desiredEyeCenter3d ||
            eyeRotation3d <> desiredEyeRotation3d

        // enable global docking
        ImGui.DockSpaceOverViewport (ImGui.GetMainViewport (), ImGuiDockNodeFlags.PassthruCentralNode) |> ignore<uint>

        // attempt to proceed with normal operation
        match recoverableExceptionOpt with
        | None ->

            // use a generalized exception process
            try

                // track state for hot key input
                let mutable entityHierarchyFocused = false

                // viewport interaction
                let io = ImGui.GetIO ()
                ImGui.SetNextWindowPos v2Zero
                ImGui.SetNextWindowSize io.DisplaySize
                if ImGui.IsKeyPressed ImGuiKey.Escape && not (modal ()) then ImGui.SetNextWindowFocus ()
                if ImGui.Begin ("Viewport", ImGuiWindowFlags.NoBackground ||| ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoInputs ||| ImGuiWindowFlags.NoNav) then

                    // guizmo manipulation
                    let viewport = Constants.Render.Viewport
                    let projectionMatrix = viewport.Projection3d Constants.Render.NearPlaneDistanceEnclosed Constants.Render.FarPlaneDistanceOmnipresent
                    let projection = projectionMatrix.ToArray ()
                    ImGuizmo.SetOrthographic false
                    ImGuizmo.SetRect (0.0f, 0.0f, io.DisplaySize.X, io.DisplaySize.Y)
                    ImGuizmo.SetDrawlist (ImGui.GetBackgroundDrawList ())
                    match selectedEntityOpt with
                    | Some entity when entity.Exists world && entity.GetIs3d world ->
                        let viewMatrix = viewport.View3d (entity.GetAbsolute world, World.getEyeCenter3d world, World.getEyeRotation3d world)
                        let view = viewMatrix.ToArray ()
                        let affineMatrix = entity.GetAffineMatrix world
                        let affine = affineMatrix.ToArray ()
                        if not manipulationActive then
                            if ImGui.IsShiftDown () then manipulationOperation <- OPERATION.SCALE
                            elif ImGui.IsAltDown () then manipulationOperation <- OPERATION.ROTATE
                            else manipulationOperation <- OPERATION.TRANSLATE
                        if ImGuizmo.Manipulate (&view.[0], &projection.[0], manipulationOperation, MODE.WORLD, &affine.[0]) then
                            if not manipulationActive && ImGui.IsMouseDown ImGuiMouseButton.Left then
                                snapshot ()
                                manipulationActive <- true
                            let affine' = Matrix4x4.CreateFromArray affine
                            let mutable (scale, rotation, position) = (v3One, quatIdentity, v3Zero)
                            if Matrix4x4.Decompose (affine', &scale, &rotation, &position) then
                                let (p, _, s) =
                                    if  not snaps2dSelected &&
                                        ImGui.IsCtrlReleased () then
                                        snaps3d else (0.0f, 0.0f, 0.0f)
                                scale <- Math.SnapF3d s scale
                                if scale.X < 0.01f then scale.X <- 0.01f
                                if scale.Y < 0.01f then scale.Y <- 0.01f
                                if scale.Z < 0.01f then scale.Z <- 0.01f
                                position <- Math.SnapF3d p position
                                match manipulationOperation with
                                | OPERATION.SCALE -> world <- entity.SetScale scale world
                                | OPERATION.ROTATE -> world <- entity.SetRotation rotation world
                                | OPERATION.TRANSLATE -> world <- entity.SetPosition position world
                                | _ -> () // nothing to do
                            if world.Advancing then
                                match entity.TryGetProperty (nameof entity.LinearVelocity) world with
                                | Some property when property.PropertyType = typeof<Vector3> -> world <- entity.SetLinearVelocity v3Zero world
                                | Some _ | None -> ()
                                match entity.TryGetProperty (nameof entity.AngularVelocity) world with
                                | Some property when property.PropertyType = typeof<Vector3> -> world <- entity.SetAngularVelocity v3Zero world
                                | Some _ | None -> ()
                        if ImGui.IsMouseReleased ImGuiMouseButton.Left then manipulationActive <- false
                    | Some _ | None -> ()

                    // view manipulation
                    // NOTE: this code is the current failed attempt to integrate ImGuizmo view manipulation as reported here - https://github.com/CedricGuillemet/ImGuizmo/issues/304
                    //let eyeCenter = (World.getEyeCenter3d world |> Matrix4x4.CreateTranslation).ToArray ()
                    //let eyeRotation = (World.getEyeRotation3d world |> Matrix4x4.CreateFromQuaternion).ToArray ()
                    //let eyeScale = m4Identity.ToArray ()
                    //let view = m4Identity.ToArray ()
                    //ImGuizmo.RecomposeMatrixFromComponents (&eyeCenter.[0], &eyeRotation.[0], &eyeScale.[0], &view.[0])
                    //ImGuizmo.ViewManipulate (&view.[0], 1.0f, v2 1400.0f 100.0f, v2 150.0f 150.0f, uint 0x10101010)
                    //ImGuizmo.DecomposeMatrixToComponents (&view.[0], &eyeCenter.[0], &eyeRotation.[0], &eyeScale.[0])
                    //eyeCenter3d <- (eyeCenter |> Matrix4x4.CreateFromArray).Translation
                    //eyeRotation3d <- (eyeRotation |> Matrix4x4.CreateFromArray |> Quaternion.CreateFromRotationMatrix)

                    // light probe bounds manipulation
                    match selectedEntityOpt with
                    | Some entity when entity.Exists world && entity.Has<LightProbeFacet3d> world ->
                        let bounds = entity.GetProbeBounds world
                        let drawList = ImGui.GetBackgroundDrawList ()
                        let eyeRotation = World.getEyeRotation3d world
                        let eyeCenter = World.getEyeCenter3d world
                        let viewport = Constants.Render.Viewport
                        let view = viewport.View3d (entity.GetAbsolute world, eyeCenter, eyeRotation)
                        let projection = viewport.Projection3d Constants.Render.NearPlaneDistanceOmnipresent Constants.Render.FarPlaneDistanceOmnipresent
                        let viewProjection = view * projection
                        let corners = Array.map (fun corner -> ImGui.PositionToWindow (viewProjection, corner)) bounds.Corners
                        let segments =
                            [|(corners.[0], corners.[1])
                              (corners.[1], corners.[2])
                              (corners.[2], corners.[3])
                              (corners.[3], corners.[0])
                              (corners.[4], corners.[5])
                              (corners.[5], corners.[6])
                              (corners.[6], corners.[7])
                              (corners.[7], corners.[4])
                              (corners.[0], corners.[6])
                              (corners.[1], corners.[5])
                              (corners.[2], corners.[4])
                              (corners.[3], corners.[7])|]
                        for (a, b) in segments do drawList.AddLine (a, b, uint 0xFF00CFCF)
                        let mutable found = false
                        for i in 0 .. dec corners.Length do
                            let corner = corners.[i]
                            if  not found &&
                                not (ImGuizmo.IsOver ()) &&
                                ImGui.IsMouseDragging ImGuiMouseButton.Left &&
                                (ImGui.GetMousePos () - corner).Magnitude < 10.0f then
                                drawList.AddCircleFilled (corner, 5.0f, uint 0xFF0000CF)
                                io.SwallowMouse ()
                                // WIP
                                //let (x, y) =
                                //    let eyeForward = eyeRotation.Forward
                                //    let dotXZ = eyeForward.Y * eyeForward.Y
                                //    let dotXY = eyeForward.Z * eyeForward.Z
                                //    let dotYZ = eyeForward.X * eyeForward.X
                                //    if dotXZ >= dotXY && dotXZ >= dotYZ then (v3Right, v3Forward)
                                //    elif dotXY >= dotXZ && dotXY >= dotYZ then (v3Right, v3Up)
                                //    else (v3Up, v3Forward)
                                //
                                //let delta = ImGui.GetMouseDragDelta ImGuiMouseButton.Left
                                //found <- true
                            else drawList.AddCircleFilled (corner, 5.0f, uint 0xFF00CFCF)
                    | _ -> ()

                    // user-defined viewport manipulation
                    match selectedEntityOpt with
                    | Some entity when entity.Exists world && entity.GetIs3d world ->
                        let viewMatrix =
                            viewport.View3d (entity.GetAbsolute world, World.getEyeCenter3d world, World.getEyeRotation3d world)
                        let operation =
                            OverlayViewport
                                { Snapshot = fun world -> snapshot (); world
                                  ViewportView = viewMatrix
                                  ViewportProjection = projectionMatrix
                                  ViewportBounds = box2 v2Zero io.DisplaySize }
                        world <- World.editEntity operation entity world
                    | Some _ | None -> ()

                    // fin
                    ImGui.End ()

                // show all windows when out in full-screen mode
                if not fullScreen then

                    // main menu window
                    if ImGui.Begin ("Gaia", ImGuiWindowFlags.MenuBar ||| ImGuiWindowFlags.NoNav) then
                        if ImGui.BeginMenuBar () then
                            if ImGui.BeginMenu "Project" then
                                if ImGui.MenuItem "New Project" then showNewProjectDialog <- true
                                if ImGui.MenuItem "Open Project" then showOpenProjectDialog <- true
                                if ImGui.MenuItem "Close Project" then showCloseProjectDialog <- true
                                ImGui.Separator ()
                                if ImGui.MenuItem "Exit" then showConfirmExitDialog <- true
                                ImGui.EndMenu ()
                            if ImGui.BeginMenu "Group" then
                                if ImGui.MenuItem ("New Group", "Ctrl+N") then showNewGroupDialog <- true
                                if ImGui.MenuItem ("Open Group", "Ctrl+O") then showOpenGroupDialog <- true
                                if ImGui.MenuItem ("Save Group", "Ctrl+S") then
                                    match Map.tryFind selectedGroup.GroupAddress groupFilePaths with
                                    | Some filePath -> groupFileDialogState.FilePath <- filePath
                                    | None -> groupFileDialogState.FileName <- ""
                                    showSaveGroupDialog <- true
                                if ImGui.MenuItem "Close Group" then
                                    let groups = world |> World.getGroups selectedScreen |> Set.ofSeq
                                    if not (selectedGroup.GetProtected world) && Set.count groups > 1 then
                                        snapshot ()
                                        let groupsRemaining = Set.remove selectedGroup groups
                                        selectEntityOpt None
                                        world <- World.destroyGroupImmediate selectedGroup world
                                        groupFilePaths <- Map.remove selectedGroup.GroupAddress groupFilePaths
                                        selectGroup (Seq.head groupsRemaining)
                                    else messageBoxOpt <- Some "Cannot close protected or only group."
                                ImGui.EndMenu ()
                            if ImGui.BeginMenu "Entity" then
                                if ImGui.MenuItem ("Cut Entity", "Ctrl+X") then tryCutSelectedEntity () |> ignore<bool>
                                if ImGui.MenuItem ("Copy Entity", "Ctrl+C") then tryCopySelectedEntity () |> ignore<bool>
                                if ImGui.MenuItem ("Paste Entity", "Ctrl+V") then tryPaste false |> ignore<bool>
                                ImGui.Separator ()
                                if ImGui.MenuItem ("Create Entity", "Ctrl+Enter") then createEntity false false
                                if ImGui.MenuItem ("Delete Entity", "Delete") then tryDeleteSelectedEntity () |> ignore<bool>
                                if ImGui.MenuItem ("Quick Size", "Ctrl+Q") then tryQuickSizeSelectedEntity () |> ignore<bool>
                                ImGui.EndMenu ()
                            if ImGui.BeginMenu "Edit" then
                                if ImGui.MenuItem ("Undo", "Ctrl+Z") then tryUndo () |> ignore<bool>
                                if ImGui.MenuItem ("Redo", "Ctrl+Y") then tryRedo () |> ignore<bool>
                                ImGui.Separator ()
                                if ImGui.MenuItem ("Mark Light Probes Stale") then markLightProbesStale ()
                                ImGui.Separator ()
                                if ImGui.MenuItem ("Run/Pause", "F5") then toggleAdvancing ()
                                if editWhileAdvancing
                                then if ImGui.MenuItem ("Disable Edit while Advancing", "F6") then editWhileAdvancing <- false
                                else if ImGui.MenuItem ("Enable Edit while Advancing", "F6") then editWhileAdvancing <- true
                                ImGui.Separator ()
                                if ImGui.MenuItem ("Reload Assets", "F8") then reloadAssetsRequested <- 1
                                if ImGui.MenuItem ("Reload Code", "F9") then reloadCodeRequested <- 1
                                if ImGui.MenuItem ("Reload All", "Ctrl+R") then reloadAllRequested <- 1
                                ImGui.EndMenu ()
                            ImGui.EndMenuBar ()
                        if ImGui.Button "Create" then createEntity false false
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 200.0f
                        if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                            for dispatcherName in (World.getEntityDispatchers world).Keys do
                                if ImGui.Selectable (dispatcherName, strEq dispatcherName newEntityDispatcherName) then
                                    newEntityDispatcherName <- dispatcherName
                            ImGui.EndCombo ()
                        ImGui.SameLine ()
                        ImGui.Text "w/ Overlay"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 150.0f
                        let overlayNames = Array.append [|"(Default Overlay)"; "(Routed Overlay)"; "(No Overlay)"|] (World.getOverlays world |> Map.toKeyArray)
                        if ImGui.BeginCombo ("##newEntityOverlayName", newEntityOverlayName) then
                            for overlayName in overlayNames do
                                if ImGui.Selectable (overlayName, strEq overlayName newEntityOverlayName) then
                                    newEntityDispatcherName <- overlayName
                            ImGui.EndCombo ()
                        ImGui.SameLine ()
                        ImGui.Text "@ Elev."
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 50.0f
                        ImGui.DragFloat ("##newEntityElevation", &newEntityElevation, snapDrag, Single.MinValue, Single.MaxValue, "%2.2f") |> ignore<bool>
                        ImGui.SameLine ()
                        if ImGui.Button "Quick Size" then tryQuickSizeSelectedEntity () |> ignore<bool>
                        ImGui.SameLine ()
                        if ImGui.Button "Delete" then tryDeleteSelectedEntity () |> ignore<bool>
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        if world.Halted then
                            if ImGui.Button "Advance (F5)" then
                                snapshot ()
                                world <- World.setAdvancing true world
                        else
                            if ImGui.Button "Halt (F5)" then
                                world <- World.setAdvancing false world
                            ImGui.SameLine ()
                            ImGui.Checkbox ("Edit", &editWhileAdvancing) |> ignore<bool>
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        ImGui.Text "Snap:"
                        ImGui.SameLine ()
                        ImGui.Text "2d"
                        ImGui.SameLine ()
                        ImGui.Checkbox ("##snaps2dSelected", &snaps2dSelected) |> ignore<bool>
                        ImGui.SameLine ()
                        let mutable (p, d, s) = if snaps2dSelected then snaps2d else snaps3d
                        ImGui.Text "Pos"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 36.0f
                        ImGui.DragFloat ("##p", &p, (if snaps2dSelected then 0.1f else 0.01f), 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
                        ImGui.SameLine ()
                        ImGui.Text "Deg"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 36.0f
                        if snaps2dSelected
                        then ImGui.DragFloat ("##d", &d, 0.1f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
                        else ImGui.DragFloat ("##d", &d, 0.0f, 0.0f, 0.0f, "%2.2f") |> ignore<bool> // unchangable 3d rotation
                        ImGui.SameLine ()
                        ImGui.Text "Scl"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 36.0f
                        ImGui.DragFloat ("##s", &s, 0.01f, 0.0f, Single.MaxValue, "%2.2f") |> ignore<bool>
                        ImGui.SameLine ()
                        if snaps2dSelected then snaps2d <- (p, d, s) else snaps3d <- (p, d, s)
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        ImGui.Text "Eye:"
                        ImGui.SameLine ()
                        if ImGui.Button "Reset" then resetEye ()
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        ImGui.Text "Reload:"
                        ImGui.SameLine ()
                        if ImGui.Button "Assets" then reloadAssetsRequested <- 1
                        ImGui.SameLine ()
                        if ImGui.Button "Code" then reloadCodeRequested <- 1
                        ImGui.SameLine ()
                        if ImGui.Button "All" then reloadAllRequested <- 1
                        ImGui.SameLine ()
                        ImGui.Text "|"
                        ImGui.SameLine ()
                        ImGui.Text "Mode:"
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth 130.0f
                        if ImGui.BeginCombo ("##projectEditMode", projectEditMode) then
                            let editModes = World.getEditModes world
                            for editMode in editModes do
                                if ImGui.Selectable (editMode.Key, strEq editMode.Key projectEditMode) then
                                    projectEditMode <- editMode.Key
                                    snapshot () // snapshot before mode change
                                    selectEntityOpt None
                                    world <- editMode.Value world
                                    snapshot () // snapshot before after change
                            ImGui.EndCombo ()
                        ImGui.SameLine ()
                        if ImGui.Button "Relight" then markLightProbesStale ()
                        if ImGui.IsItemHovered () then
                            if ImGui.BeginTooltip () then
                                ImGui.Text "Re-render all light maps."
                                ImGui.EndTooltip ()
                        ImGui.SameLine ()
                        ImGui.Text "Full (F11)"
                        ImGui.SameLine ()
                        ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                        ImGui.End ()

                    // asset viewer window
                    if ImGui.Begin "Asset Viewer" then
                        ImGui.SetNextItemWidth -1.0f
                        ImGui.InputTextWithHint ("##assetViewerSearchStr", "[enter search text]", &assetViewerSearchStr, 4096u) |> ignore<bool>
                        let assets = Metadata.getDiscoveredAssets ()
                        for package in assets do
                            let flags = ImGuiTreeNodeFlags.SpanAvailWidth ||| ImGuiTreeNodeFlags.OpenOnArrow
                            if ImGui.TreeNodeEx (package.Key, flags) then
                                for assetName in package.Value do
                                    if (assetName.ToLowerInvariant ()).Contains (assetViewerSearchStr.ToLowerInvariant ()) then
                                        ImGui.TreeNodeEx (assetName, flags ||| ImGuiTreeNodeFlags.Leaf) |> ignore<bool>
                                        if ImGui.BeginDragDropSource () then
                                            let assetTagStr = "[" + package.Key + " " + assetName + "]"
                                            dragDropPayloadOpt <- Some assetTagStr
                                            ImGui.Text assetTagStr
                                            ImGui.SetDragDropPayload ("Asset", IntPtr.Zero, 0u) |> ignore<bool>
                                            ImGui.EndDragDropSource ()
                                        ImGui.TreePop ()
                                ImGui.TreePop ()
                        ImGui.End ()

                    // entity hierarchy window
                    if ImGui.Begin "Entity Hierarchy" then
                        entityHierarchyFocused <- ImGui.IsWindowFocused ()
                        if ImGui.Button "Collapse" then
                            collapseEntityHierarchy <- true
                            ImGui.SetWindowFocus "Viewport"
                        ImGui.SameLine ()
                        if ImGui.Button "Expand" then
                            expandEntityHierarchy <- true
                            ImGui.SetWindowFocus "Viewport"
                        ImGui.SameLine ()
                        if ImGui.Button "Show Selected" then
                            showSelectedEntity <- true
                            ImGui.SetWindowFocus "Viewport"
                        let groups = World.getGroups selectedScreen world
                        let mutable selectedGroupName = selectedGroup.Name
                        ImGui.SetNextItemWidth -1.0f
                        if ImGui.BeginCombo ("##selectedGroupName", selectedGroupName) then
                            for group in groups do
                                if ImGui.Selectable (group.Name, strEq group.Name selectedGroupName) then
                                    selectEntityOpt None
                                    selectGroup group
                            ImGui.EndCombo ()
                        if ImGui.BeginDragDropTarget () then
                            if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Entity").NativePtr) then
                                match dragDropPayloadOpt with
                                | Some payload ->
                                    let sourceEntityAddressStr = payload
                                    let sourceEntity = Nu.Entity sourceEntityAddressStr
                                    if not (sourceEntity.GetProtected world) then
                                        let sourceEntity' = Nu.Entity (selectedGroup.GroupAddress <-- Address.makeFromName sourceEntity.Name)
                                        world <- sourceEntity.SetMountOptWithAdjustment None world
                                        world <- World.renameEntityImmediate sourceEntity sourceEntity' world
                                        selectEntityOpt (Some sourceEntity')
                                        showSelectedEntity <- true
                                | None -> ()
                        let entities =
                            World.getEntitiesSovereign selectedGroup world |> // TODO: P1: see if we can optimize entity hierarchy queries!
                            Seq.map (fun entity -> ((entity.Surnames.Length, entity.GetOrder world), entity)) |>
                            Array.ofSeq |>
                            Array.sortBy fst |>
                            Array.map snd
                        for entity in entities do
                            imGuiEntityHierarchy entity
                        ImGui.End ()
                    else entityHierarchyFocused <- false
                    expandEntityHierarchy <- false
                    collapseEntityHierarchy <- false

                    // game properties window
                    if ImGui.Begin ("Game Properties", ImGuiWindowFlags.NoNav) then
                        imGuiEditProperties Game
                        ImGui.End ()

                    // screen properties window
                    if ImGui.Begin ("Screen Properties", ImGuiWindowFlags.NoNav) then
                        imGuiEditProperties selectedScreen
                        ImGui.End ()

                    // group properties window
                    if ImGui.Begin ("Group Properties", ImGuiWindowFlags.NoNav) then
                        imGuiEditProperties selectedGroup
                        ImGui.End ()

                    // entity properties window
                    if ImGui.Begin ("Entity Properties", ImGuiWindowFlags.NoNav) then
                        match selectedEntityOpt with
                        | Some entity when entity.Exists world -> imGuiEditProperties entity
                        | Some _ | None -> ()
                        ImGui.End ()

                    // property editor window
                    if focusPropertyEditorRequested then ImGui.SetNextWindowFocus ()
                    if ImGui.Begin ("Property Editor", ImGuiWindowFlags.NoNav) then
                        match focusedPropertyDescriptorOpt with
                        | Some (propertyDescriptor, simulant) when
                            World.getExists simulant world &&
                            propertyDescriptor.PropertyType <> typeof<ComputedProperty> ->
                            toSymbolMemo.Evict Constants.Gaia.PropertyValueStrMemoEvictionAge
                            ofSymbolMemo.Evict Constants.Gaia.PropertyValueStrMemoEvictionAge
                            let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType, toSymbolMemo, ofSymbolMemo)
                            let propertyValueUntruncated = getPropertyValue propertyDescriptor simulant
                            let propertyValue =
                                if propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                                    match World.tryTruncateModel propertyValueUntruncated simulant world with
                                    | Some truncatedValue -> truncatedValue
                                    | None -> propertyValueUntruncated
                                else propertyValueUntruncated
                            ImGui.Text propertyDescriptor.PropertyName
                            ImGui.SameLine ()
                            ImGui.Text ":"
                            ImGui.SameLine ()
                            ImGui.Text (Reflection.getSimplifiedTypeNameHack propertyDescriptor.PropertyType)
                            let propertyValueSymbol = converter.ConvertTo (propertyValue, typeof<Symbol>) :?> Symbol
                            let mutable propertyValueStr = PrettyPrinter.prettyPrintSymbol propertyValueSymbol PrettyPrinter.defaultPrinter
                            let isPropertyAssetTag = propertyDescriptor.PropertyType.IsGenericType && propertyDescriptor.PropertyType.GetGenericTypeDefinition () = typedefof<_ AssetTag>
                            if  isPropertyAssetTag then
                                ImGui.SameLine ()
                                if ImGui.Button "Pick" then showAssetPickerDialog <- true
                            if  focusPropertyEditorRequested then
                                ImGui.SetKeyboardFocusHere ()
                                focusPropertyEditorRequested <- false
                            if  propertyDescriptor.PropertyName = Constants.Engine.FacetNamesPropertyName &&
                                propertyDescriptor.PropertyType = typeof<string Set> then
                                ImGui.InputTextMultiline ("##propertyValuePretty", &propertyValueStr, 4096u, v2 -1.0f -1.0f, ImGuiInputTextFlags.ReadOnly) |> ignore<bool>
                            elif ImGui.InputTextMultiline ("##propertyValuePretty", &propertyValueStr, 131072u, v2 -1.0f -1.0f) && propertyValueStr <> propertyValueStrPrevious then
                                let worldsPast' = worldsPast
                                try let propertyValueEscaped = propertyValueStr
                                    let propertyValueUnescaped = String.unescape propertyValueEscaped
                                    let propertyValueTruncated = converter.ConvertFromString propertyValueUnescaped
                                    let propertyValue =
                                        if propertyDescriptor.PropertyName = Constants.Engine.ModelPropertyName then
                                            match World.tryUntruncateModel propertyValueTruncated simulant world with
                                            | Some truncatedValue -> truncatedValue
                                            | None -> propertyValueTruncated
                                        else propertyValueTruncated
                                    setPropertyValue propertyValue propertyDescriptor simulant
                                with _ ->
                                    worldsPast <- worldsPast'
                                propertyValueStrPrevious <- propertyValueStr
                            if isPropertyAssetTag then
                                if ImGui.BeginDragDropTarget () then
                                    if not (NativePtr.isNullPtr (ImGui.AcceptDragDropPayload "Asset").NativePtr) then
                                        match dragDropPayloadOpt with
                                        | Some payload ->
                                            let worldsPast' = worldsPast
                                            try let propertyValueEscaped = payload
                                                let propertyValueUnescaped = String.unescape propertyValueEscaped
                                                let propertyValue = converter.ConvertFromString propertyValueUnescaped
                                                setPropertyValue propertyValue propertyDescriptor simulant
                                            with _ ->
                                                worldsPast <- worldsPast'
                                        | None -> ()
                                    ImGui.EndDragDropTarget ()
                        | Some _ | None -> ()
                        ImGui.End ()

                    // asset graph window
                    if ImGui.Begin ("Asset Graph", ImGuiWindowFlags.NoNav) then
                        if ImGui.Button "Save" then
                            let assetSourceDir = targetDir + "/../../.."
                            let assetGraphFilePath = assetSourceDir + "/" + Assets.Global.AssetGraphFilePath
                            try let packageDescriptorsStr = assetGraphStr |> scvalue<Map<string, PackageDescriptor>> |> scstring
                                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                                File.WriteAllText (assetGraphFilePath, PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter)
                            with exn ->messageBoxOpt <- Some ("Could not save asset graph due to: " + scstring exn)
                        ImGui.SameLine ()
                        if ImGui.Button "Load" then
                            match AssetGraph.tryMakeFromFile (targetDir + "/" + Assets.Global.AssetGraphFilePath) with
                            | Right assetGraph ->
                                let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
                                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                                assetGraphStr <- PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
                            | Left error ->messageBoxOpt <- Some ("Could not read asset graph due to: " + error + "'.")
                        ImGui.InputTextMultiline ("##assetGraphStr", &assetGraphStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
                        ImGui.End ()

                    // overlayer window
                    if ImGui.Begin ("Overlayer", ImGuiWindowFlags.NoNav) then
                        if ImGui.Button "Save" then
                            let overlayerSourceDir = targetDir + "/../../.."
                            let overlayerFilePath = overlayerSourceDir + "/" + Assets.Global.AssetGraphFilePath
                            try let overlays = scvalue<Overlay list> overlayerStr
                                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                                File.WriteAllText (overlayerFilePath, PrettyPrinter.prettyPrint (scstring overlays) prettyPrinter)
                            with exn ->messageBoxOpt <- Some ("Could not save asset graph due to: " + scstring exn)
                        ImGui.SameLine ()
                        if ImGui.Button "Load" then
                            let overlayerFilePath = targetDir + "/" + Assets.Global.OverlayerFilePath
                            match Overlayer.tryMakeFromFile [] overlayerFilePath with
                            | Right overlayer ->
                                let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
                                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                                overlayerStr <- PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
                            | Left error ->messageBoxOpt <- Some ("Could not read overlayer due to: " + error + "'.")
                        ImGui.InputTextMultiline ("##overlayerStr", &overlayerStr, 131072u, v2 -1.0f -1.0f) |> ignore<bool>
                        ImGui.End ()

                    // event tracing window
                    if ImGui.Begin ("Event Tracing", ImGuiWindowFlags.NoNav) then
                        let mutable traceEvents = world |> World.getEventTracerOpt |> Option.isSome
                        if ImGui.Checkbox ("Trace Events", &traceEvents) then
                            world <- World.setEventTracerOpt (if traceEvents then Some (Log.remark "Event") else None) world
                        let eventFilter = World.getEventFilter world
                        let prettyPrinter = (SyntaxAttribute.defaultValue typeof<EventFilter>).PrettyPrinter
                        let mutable eventFilterStr = PrettyPrinter.prettyPrint (scstring eventFilter) prettyPrinter
                        if ImGui.InputTextMultiline ("##eventFilterStr", &eventFilterStr, 131072u, v2 -1.0f -1.0f) then
                            try let eventFilter = scvalue<EventFilter> eventFilterStr
                                world <- World.setEventFilter eventFilter world
                            with _ -> ()
                        ImGui.End ()

                    // renderer window
                    if ImGui.Begin ("Renderer", ImGuiWindowFlags.NoNav) then
                        ImGui.Text "Light-Mapping (local light mapping)"
                        let mutable lightMappingEnabled = lightMappingConfig.LightMappingEnabled
                        ImGui.Checkbox ("Light-Mapping Enabled", &lightMappingEnabled) |> ignore<bool>
                        lightMappingConfig <- { LightMappingEnabled = lightMappingEnabled }
                        world <- World.enqueueRenderMessage3d (ConfigureLightMapping lightMappingConfig) world
                        ImGui.Text "Ssao (screen-space ambient occlusion)"
                        let mutable ssaoEnabled = ssaoConfig.SsaoEnabled
                        let mutable ssaoIntensity = ssaoConfig.SsaoIntensity
                        let mutable ssaoBias = ssaoConfig.SsaoBias
                        let mutable ssaoRadius = ssaoConfig.SsaoRadius
                        let mutable ssaoDistanceMax = ssaoConfig.SsaoDistanceMax
                        let mutable ssaoSampleCount = ssaoConfig.SsaoSampleCount
                        ImGui.Checkbox ("Ssao Enabled", &ssaoEnabled) |> ignore<bool>
                        if ssaoEnabled then
                            ImGui.SliderFloat ("Ssao Intensity", &ssaoIntensity, 0.0f, 10.0f) |> ignore<bool>
                            ImGui.SliderFloat ("Ssao Bias", &ssaoBias, 0.0f, 0.1f) |> ignore<bool>
                            ImGui.SliderFloat ("Ssao Radius", &ssaoRadius, 0.0f, 1.0f) |> ignore<bool>
                            ImGui.SliderFloat ("Ssao Distance Max", &ssaoDistanceMax, 0.0f, 1.0f) |> ignore<bool>
                            ImGui.SliderInt ("Ssao Sample Count", &ssaoSampleCount, 0, Constants.Render.SsaoSampleCountMax) |> ignore<bool>
                        ssaoConfig <-
                            { SsaoEnabled = ssaoEnabled
                              SsaoIntensity = ssaoIntensity
                              SsaoBias = ssaoBias
                              SsaoRadius = ssaoRadius
                              SsaoDistanceMax = ssaoDistanceMax
                              SsaoSampleCount = ssaoSampleCount }
                        world <- World.enqueueRenderMessage3d (ConfigureSsao ssaoConfig) world
                        ImGui.End ()

                    // audio player window
                    if ImGui.Begin ("Audio Player", ImGuiWindowFlags.NoNav) then
                        ImGui.Text "Master Sound Volume"
                        let mutable masterSoundVolume = World.getMasterSoundVolume world
                        if ImGui.SliderFloat ("##masterSoundVolume", &masterSoundVolume, 0.0f, 1.0f) then world <- World.setMasterSoundVolume masterSoundVolume world
                        ImGui.SameLine ()
                        ImGui.Text (string masterSoundVolume)
                        ImGui.Text "Master Song Volume"
                        let mutable masterSongVolume = World.getMasterSongVolume world
                        if ImGui.SliderFloat ("##masterSongVolume", &masterSongVolume, 0.0f, 1.0f) then world <- World.setMasterSongVolume masterSongVolume world
                        ImGui.SameLine ()
                        ImGui.Text (string masterSongVolume)
                        ImGui.End ()

                // in full-screen mode, just show full-screen short cut window
                else
                    if ImGui.Begin ("Full Screen Enabled", ImGuiWindowFlags.NoNav) then
                        ImGui.Text "Full Screen (F11)"
                        ImGui.SameLine ()
                        ImGui.Checkbox ("##fullScreen", &fullScreen) |> ignore<bool>
                        ImGui.End ()

                // if message box not shown, may show another popup
                match messageBoxOpt with
                | None ->

                    // asset picker dialog
                    if showAssetPickerDialog then
                        let title = "Choose an Asset..."
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showAssetPickerDialog) then
                            ImGui.SetNextItemWidth -1.0f
                            ImGui.InputTextWithHint ("##searchString", "[enter search text]", &assetPickerSearchStr, 4096u) |> ignore<bool>
                            let assets = Metadata.getDiscoveredAssets ()
                            for package in assets do
                                let flags = ImGuiTreeNodeFlags.SpanAvailWidth ||| ImGuiTreeNodeFlags.OpenOnArrow
                                if ImGui.TreeNodeEx (package.Key, flags) then
                                    for assetName in package.Value do
                                        if (assetName.ToLowerInvariant ()).Contains (assetPickerSearchStr.ToLowerInvariant ()) then
                                            if ImGui.TreeNodeEx (assetName, flags ||| ImGuiTreeNodeFlags.Leaf) then
                                                if ImGui.IsMouseDoubleClicked ImGuiMouseButton.Left && ImGui.IsItemHovered () then
                                                    match focusedPropertyDescriptorOpt with
                                                    | Some (propertyDescriptor, simulant) when
                                                        World.getExists simulant world &&
                                                        propertyDescriptor.PropertyType <> typeof<ComputedProperty> ->
                                                        let converter = SymbolicConverter (false, None, propertyDescriptor.PropertyType)
                                                        let propertyValueStr = "[" + package.Key + " " + assetName + "]"
                                                        let propertyValue = converter.ConvertFromString propertyValueStr
                                                        setPropertyValue propertyValue propertyDescriptor simulant
                                                    | Some _ | None -> ()
                                                    showAssetPickerDialog <- false
                                                ImGui.TreePop ()
                                    ImGui.TreePop ()
                            if ImGui.IsKeyPressed ImGuiKey.Escape then showAssetPickerDialog <- false
                            ImGui.EndPopup ()

                    // new project dialog
                    if showNewProjectDialog then

                        // ensure template directory exists
                        let programDir = Reflection.Assembly.GetEntryAssembly().Location |> Path.GetDirectoryName |> fun dir -> dir.Replace ("\\", "/")
                        let slnDir = programDir + "/../../../../.." |> Path.GetFullPath |> fun dir -> dir.Replace ("\\", "/")
                        let templateDir = programDir + "/../../../../Nu.Template" |> Path.GetFullPath |> fun dir -> dir.Replace ("\\", "/")
                        if Directory.Exists templateDir then

                            // prompt user to create new project
                            let title = "Create Nu Project... *EDITOR RESTART REQUIRED!*"
                            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                            if ImGui.BeginPopupModal (title, &showNewProjectDialog) then
                                ImGui.Text "Project Name"
                                ImGui.SameLine ()
                                ImGui.InputText ("##newProjectName", &newProjectName, 4096u) |> ignore<bool>
                                newProjectName <- newProjectName.Replace(" ", "").Replace("\t", "").Replace(".", "")
                                let templateIdentifier = templateDir.Replace ("/", "\\") // this is what dotnet knows the template as for uninstall...
                                let templateFileName = "Nu.Template.fsproj"
                                let projectsDir = programDir + "/../../../../../Projects" |> Path.GetFullPath |> fun dir -> dir.Replace ("\\", "/")
                                let newProjectDir = projectsDir + "/" + newProjectName |> Path.GetFullPath |> fun dir -> dir.Replace ("\\", "/")
                                let newProjectDllPath = newProjectDir + "/bin/" + Constants.Gaia.BuildName + "/net7.0/" + newProjectName + ".dll"
                                let newFileName = newProjectName + ".fsproj"
                                let newProject = newProjectDir + "/" + newFileName |> Path.GetFullPath |> fun dir -> dir.Replace ("\\", "/")
                                let validName = not (String.IsNullOrWhiteSpace newProjectName) && Array.notExists (fun char -> newProjectName.Contains (string char)) (Path.GetInvalidPathChars ())
                                if not validName then ImGui.Text "Invalid project name!"
                                let validDirectory = not (Directory.Exists newProjectDir)
                                if not validDirectory then ImGui.Text "Project already exists!"
                                if validName && validDirectory && (ImGui.Button "Create" || ImGui.IsKeyPressed ImGuiKey.Enter) then

                                    // attempt to create project files
                                    try Log.info ("Creating project '" + newProjectName + "' in '" + projectsDir + "'...")

                                        // install nu template
                                        Directory.SetCurrentDirectory templateDir
                                        Process.Start("dotnet", "new uninstall \"" + templateIdentifier + "\"").WaitForExit()
                                        Process.Start("dotnet", "new install ./").WaitForExit()

                                        // instantiate nu template
                                        Directory.SetCurrentDirectory projectsDir
                                        Directory.CreateDirectory newProjectName |> ignore<DirectoryInfo>
                                        Directory.SetCurrentDirectory newProjectDir
                                        Process.Start("dotnet", "new nu-game --force").WaitForExit()

                                        // rename project file
                                        File.Copy (templateFileName, newFileName, true)
                                        File.Delete templateFileName

                                        // substitute project guid in project file
                                        let projectGuid = Gen.id
                                        let projectGuidStr = projectGuid.ToString().ToUpperInvariant()
                                        let newProjectStr = File.ReadAllText newProject
                                        let newProjectStr = newProjectStr.Replace("4DBBAA23-56BA-43CB-AB63-C45D5FC1016F", projectGuidStr)
                                        File.WriteAllText (newProject, newProjectStr)

                                        // add project to sln file
                                        Directory.SetCurrentDirectory slnDir
                                        let slnLines = "Nu.sln" |> File.ReadAllLines |> Array.toList
                                        let insertionIndex = List.findIndexBack ((=) "\tEndProjectSection") slnLines
                                        let slnLines = 
                                            List.take insertionIndex slnLines @
                                            ["\t\t{" + projectGuidStr + "} = {" + projectGuidStr + "}"] @
                                            List.skip insertionIndex slnLines
                                        let insertionIndex = List.findIndex ((=) "Global") slnLines
                                        let slnLines =
                                            List.take insertionIndex slnLines @
                                            ["Project(\"{6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705}\") = \"" + newProjectName + "\", \"Projects\\" + newProjectName + "\\" + newProjectName + ".fsproj\", \"{" + projectGuidStr + "}\""
                                             "EndProject"] @
                                            List.skip insertionIndex slnLines
                                        let insertionIndex = List.findIndex ((=) "\tGlobalSection(SolutionProperties) = preSolution") slnLines - 1
                                        let slnLines =
                                            List.take insertionIndex slnLines @
                                            ["\t\t{" + projectGuidStr + "}.Debug|Any CPU.ActiveCfg = Debug|Any CPU"
                                             "\t\t{" + projectGuidStr + "}.Debug|Any CPU.Build.0 = Debug|Any CPU"
                                             "\t\t{" + projectGuidStr + "}.Release|Any CPU.ActiveCfg = Release|Any CPU"
                                             "\t\t{" + projectGuidStr + "}.Release|Any CPU.Build.0 = Release|Any CPU"] @
                                            List.skip insertionIndex slnLines
                                        let insertionIndex = List.findIndex ((=) "\tGlobalSection(ExtensibilityGlobals) = postSolution") slnLines - 1
                                        let slnLines =
                                            List.take insertionIndex slnLines @
                                            ["\t\t{" + projectGuidStr + "} = {E3C4D6E1-0572-4D80-84A9-8001C21372D3}"] @
                                            List.skip insertionIndex slnLines
                                        File.WriteAllLines ("Nu.sln", List.toArray slnLines)
                                        Log.info ("Project '" + newProjectName + "'" + "created.")

                                        // configure editor to open new project then exit
                                        let gaiaState = GaiaState.make newProjectDllPath (Some "Title") openProjectImperativeExecution true desiredEyeCenter2d desiredEyeCenter3d desiredEyeRotation3d (World.getMasterSoundVolume world) (World.getMasterSongVolume world)
                                        let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                        let gaiaDirectory = Path.GetDirectoryName gaiaFilePath
                                        try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, scstring gaiaState)
                                            Directory.SetCurrentDirectory gaiaDirectory
                                            showRestartDialog <- true
                                        with _ -> Log.trace "Could not save gaia state and open new project."

                                        // close dialog
                                        showNewProjectDialog <- false
                                        newProjectName <- "MyGame"

                                    // log failure
                                    with exn -> Log.trace ("Failed to create new project '" + newProjectName + "' due to: " + scstring exn)

                                // escape to cancel
                                if ImGui.IsKeyPressed ImGuiKey.Escape then
                                    showNewProjectDialog <- false
                                    newProjectName <- "MyGame"

                                // fin
                                ImGui.EndPopup ()

                        // template project missing
                        else
                            Log.trace "Template project is missing; new project cannot be generated."
                            showNewProjectDialog <- false

                    // open project dialog
                    if showOpenProjectDialog && not showOpenProjectFileDialog then
                        let title = "Choose a project .dll... *EDITOR RESTART REQUIRED!*"
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showOpenProjectDialog) then
                            ImGui.Text "Game Assembly Path:"
                            ImGui.SameLine ()
                            ImGui.InputTextWithHint ("##openProjectFilePath", "[enter game .dll path]", &openProjectFilePath, 4096u) |> ignore<bool>
                            ImGui.SameLine ()
                            if ImGui.Button "..." then showOpenProjectFileDialog <- true
                            ImGui.Text "Edit Mode:"
                            ImGui.SameLine ()
                            ImGui.InputText ("##openProjectEditMode", &openProjectEditMode, 4096u) |> ignore<bool>
                            ImGui.Checkbox ("Use Imperative Execution (faster, but no Undo / Redo)", &openProjectImperativeExecution) |> ignore<bool>
                            if  (ImGui.Button "Open" || ImGui.IsKeyPressed ImGuiKey.Enter) &&
                                String.notEmpty openProjectFilePath &&
                                File.Exists openProjectFilePath then
                                showOpenProjectDialog <- false
                                let gaiaState = GaiaState.make openProjectFilePath (Some openProjectEditMode) openProjectImperativeExecution true desiredEyeCenter2d desiredEyeCenter3d desiredEyeRotation3d (World.getMasterSoundVolume world) (World.getMasterSongVolume world)
                                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                let gaiaDirectory = Path.GetDirectoryName gaiaFilePath
                                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, scstring gaiaState)
                                    Directory.SetCurrentDirectory gaiaDirectory
                                    showRestartDialog <- true
                                with _ -> Log.info "Could not save editor state and open project."
                            if ImGui.IsKeyPressed ImGuiKey.Escape then showOpenProjectDialog <- false
                            ImGui.EndPopup ()

                    // open project file dialog
                    elif showOpenProjectFileDialog then
                        projectFileDialogState.Title <- "Choose a game .dll..."
                        projectFileDialogState.FilePattern <- "*.dll"
                        projectFileDialogState.FileDialogType <- ImGuiFileDialogType.Open
                        if ImGui.FileDialog (&showOpenProjectFileDialog, projectFileDialogState) then
                            openProjectFilePath <- projectFileDialogState.FilePath

                    // close project dialog
                    if showCloseProjectDialog then
                        let title = "Close project... *EDITOR RESTART REQUIRED!*"
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showCloseProjectDialog) then
                            ImGui.Text "Close the project and use Gaia in its default state?"
                            if ImGui.Button "Okay" || ImGui.IsKeyPressed ImGuiKey.Enter then
                                showCloseProjectDialog <- false
                                let gaiaState = GaiaState.defaultState
                                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                let gaiaDirectory = Path.GetDirectoryName gaiaFilePath
                                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, scstring gaiaState)
                                    Directory.SetCurrentDirectory gaiaDirectory
                                    showRestartDialog <- true
                                with _ -> Log.info "Could not clear editor state and close project."
                            if ImGui.IsKeyPressed ImGuiKey.Escape then showCloseProjectDialog <- false
                            ImGui.EndPopup ()

                    // new group dialog
                    if showNewGroupDialog then
                        let title = "Create a group..."
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showNewGroupDialog) then
                            ImGui.Text "Group Name:"
                            ImGui.SameLine ()
                            ImGui.InputTextWithHint ("##newGroupName", "[enter group name]", &newGroupName, 4096u) |> ignore<bool>
                            let newGroup = selectedScreen / newGroupName
                            if ImGui.BeginCombo ("##newGroupDispatcherName", newGroupDispatcherName) then
                                for dispatcherName in (World.getGroupDispatchers world).Keys do
                                    if ImGui.Selectable (dispatcherName, strEq dispatcherName newGroupDispatcherName) then
                                        newGroupDispatcherName <- dispatcherName
                                ImGui.EndCombo ()
                            if (ImGui.Button "Create" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty newGroupName && Address.validName newGroupName && not (newGroup.Exists world) then
                                let worldOld = world
                                try world <- World.createGroup4 newGroupDispatcherName (Some newGroupName) selectedScreen world |> snd
                                    selectEntityOpt None
                                    selectGroup newGroup
                                    showNewGroupDialog <- false
                                    newGroupName <- ""
                                with exn ->
                                    world <- World.choose worldOld
                                    messageBoxOpt <- Some ("Could not create group due to: " + scstring exn)
                            if ImGui.IsKeyPressed ImGuiKey.Escape then showNewGroupDialog <- false
                            ImGui.EndPopup ()

                    // open group dialog
                    if showOpenGroupDialog then
                        groupFileDialogState.Title <- "Choose a nugroup file..."
                        groupFileDialogState.FilePattern <- "*.nugroup"
                        groupFileDialogState.FileDialogType <- ImGuiFileDialogType.Open
                        if ImGui.FileDialog (&showOpenGroupDialog, groupFileDialogState) then
                            snapshot ()
                            showOpenGroupDialog <- not (tryLoadSelectedGroup groupFileDialogState.FilePath)

                    // save group dialog
                    if showSaveGroupDialog then
                        groupFileDialogState.Title <- "Save a nugroup file..."
                        groupFileDialogState.FilePattern <- "*.nugroup"
                        groupFileDialogState.FileDialogType <- ImGuiFileDialogType.Save
                        if ImGui.FileDialog (&showSaveGroupDialog, groupFileDialogState) then
                            snapshot ()
                            if not (Path.HasExtension groupFileDialogState.FilePath) then groupFileDialogState.FilePath <- groupFileDialogState.FilePath + ".nugroup"
                            showSaveGroupDialog <- not (trySaveSelectedGroup groupFileDialogState.FilePath)

                    // rename group dialog
                    if showRenameGroupDialog then
                        match selectedGroup with
                        | group when group.Exists world ->
                            let title = "Rename group..."
                            let opening = not (ImGui.IsPopupOpen title)
                            if opening then ImGui.OpenPopup title
                            if ImGui.BeginPopupModal (title, &showRenameGroupDialog) then
                                groupRename <- group.Name
                                ImGui.Text "Group Name:"
                                ImGui.SameLine ()
                                if opening then ImGui.SetKeyboardFocusHere ()
                                ImGui.InputTextWithHint ("##groupName", "[enter group name]", &groupRename, 4096u) |> ignore<bool>
                                let group' = group.Screen / groupRename
                                if (ImGui.Button "Apply" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty groupRename && Address.validName groupRename && not (group'.Exists world) then
                                    snapshot ()
                                    world <- World.renameGroupImmediate group group' world
                                    selectedGroup <- group'
                                    showRenameGroupDialog <- false
                                if ImGui.IsKeyPressed ImGuiKey.Escape then showRenameGroupDialog <- false
                                ImGui.EndPopup ()
                        | _ -> showRenameGroupDialog <- false

                    // rename entity dialog
                    if showRenameEntityDialog then
                        match selectedEntityOpt with
                        | Some entity when entity.Exists world ->
                            let title = "Rename entity..."
                            let opening = not (ImGui.IsPopupOpen title)
                            if opening then ImGui.OpenPopup title
                            if ImGui.BeginPopupModal (title, &showRenameEntityDialog) then
                                entityRename <- entity.Name
                                ImGui.Text "Entity Name:"
                                ImGui.SameLine ()
                                if opening then ImGui.SetKeyboardFocusHere ()
                                ImGui.InputTextWithHint ("##entityRename", "[enter entity name]", &entityRename, 4096u) |> ignore<bool>
                                let entity' = Nu.Entity (Array.add entityRename entity.Parent.SimulantAddress.Names)
                                if (ImGui.Button "Apply" || ImGui.IsKeyPressed ImGuiKey.Enter) && String.notEmpty entityRename && Address.validName entityRename && not (entity'.Exists world) then
                                    snapshot ()
                                    world <- World.renameEntityImmediate entity entity' world
                                    selectedEntityOpt <- Some entity'
                                    showRenameEntityDialog <- false
                                if ImGui.IsKeyPressed ImGuiKey.Escape then showRenameEntityDialog <- false
                                ImGui.EndPopup ()
                        | Some _ | None -> showRenameEntityDialog <- false

                    // confirm exit dialog
                    if showConfirmExitDialog then
                        let title = "Are you okay with exiting Gaia?"
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal (title, &showConfirmExitDialog) then
                            ImGui.Text "Any unsaved changes will be lost."
                            if ImGui.Button "Okay" || ImGui.IsKeyPressed ImGuiKey.Enter then
                                let gaiaState = GaiaState.make projectDllPath (Some projectEditMode) projectImperativeExecution false desiredEyeCenter2d desiredEyeCenter3d desiredEyeRotation3d (World.getMasterSoundVolume world) (World.getMasterSongVolume world)
                                let gaiaFilePath = (Assembly.GetEntryAssembly ()).Location
                                let gaiaDirectory = Path.GetDirectoryName gaiaFilePath
                                try File.WriteAllText (gaiaDirectory + "/" + Constants.Gaia.StateFilePath, scstring gaiaState)
                                    Directory.SetCurrentDirectory gaiaDirectory
                                with _ -> Log.trace "Could not save gaia state."
                                world <- World.exit world
                            ImGui.SameLine ()
                            if ImGui.Button "Cancel" || ImGui.IsKeyPressed ImGuiKey.Escape then showConfirmExitDialog <- false
                            ImGui.EndPopup ()

                    // restart dialog
                    if showRestartDialog then
                        let title = "Editor restart required."
                        if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                        if ImGui.BeginPopupModal title then
                            ImGui.Text "Gaia will apply your configuration changes and exit. Restart Gaia after exiting."
                            if ImGui.Button "Okay" || ImGui.IsKeyPressed ImGuiKey.Enter then world <- World.exit world
                            ImGui.EndPopup ()
                
                // message box dialog
                | Some messageBox ->
                    let title = "Message!"
                    let mutable showing = true
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal (title, &showing) then
                        ImGui.TextWrapped messageBox
                        if ImGui.Button "Okay" || ImGui.IsKeyPressed ImGuiKey.Enter || ImGui.IsKeyPressed ImGuiKey.Escape then showing <- false
                        if not showing then messageBoxOpt <- None
                        ImGui.EndPopup ()

                // entity context menu
                if showEntityContextMenu then
                    ImGui.SetNextWindowPos rightClickPosition
                    ImGui.SetNextWindowSize (v2 250.0f 158.0f)
                    if ImGui.Begin ("ContextMenu", ImGuiWindowFlags.NoTitleBar ||| ImGuiWindowFlags.NoResize) then
                        if ImGui.Button "Create" then
                            createEntity true false
                            showEntityContextMenu <- false
                        ImGui.SameLine ()
                        ImGui.SetNextItemWidth -1.0f
                        if ImGui.BeginCombo ("##newEntityDispatcherName", newEntityDispatcherName) then
                            for dispatcherName in (World.getEntityDispatchers world).Keys do
                                if ImGui.Selectable (dispatcherName, strEq dispatcherName newEntityDispatcherName) then
                                    newEntityDispatcherName <- dispatcherName
                                    createEntity true false
                                    showEntityContextMenu <- false
                            ImGui.EndCombo ()
                        if ImGui.Button "Delete" then tryDeleteSelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if  ImGui.IsMouseClicked ImGuiMouseButton.Right ||
                            ImGui.IsKeyPressed ImGuiKey.Escape then
                            showEntityContextMenu <- false
                        ImGui.Separator ()
                        if ImGui.Button "Cut" then tryCutSelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if ImGui.Button "Copy" then tryCopySelectedEntity () |> ignore<bool>; showEntityContextMenu <- false
                        if ImGui.Button "Paste" then tryPaste true |> ignore<bool>; showEntityContextMenu <- false
                        ImGui.Separator ()
                        if ImGui.Button "Show in Hierarchy" then showSelectedEntity <- true; showEntityContextMenu <- false
                        ImGui.End ()

                // imgui inspector window
                if showInspector then
                    ImGui.ShowStackToolWindow ()

                // process non-widget mouse input and hotkeys
                updateEyeDrag ()
                updateEyeTravel ()
                updateEntityContext ()
                updateEntityDrag ()
                updateHotkeys entityHierarchyFocused
                if not eyeChangedElsewhere then
                    world <- World.setEyeCenter2d desiredEyeCenter2d world
                    world <- World.setEyeCenter3d desiredEyeCenter3d world
                    world <- World.setEyeRotation3d desiredEyeRotation3d world
                else
                    desiredEyeCenter2d <- World.getEyeCenter2d world
                    desiredEyeCenter3d <- World.getEyeCenter3d world
                    desiredEyeRotation3d <- World.getEyeRotation3d world

                // reloading assets dialog
                if reloadAssetsRequested > 0 then
                    let title = "Reloading assets..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal title then
                        ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
                        ImGui.EndPopup ()
                    reloadAssetsRequested <- inc reloadAssetsRequested
                    if reloadAssetsRequested = 4 then // NOTE: takes multiple frames to see dialog.
                        tryReloadAssets ()
                        reloadAssetsRequested <- 0

                // reloading code dialog
                if reloadCodeRequested > 0 then
                    let title = "Reloading code..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal title then
                        ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
                        ImGui.EndPopup ()
                    reloadCodeRequested <- inc reloadCodeRequested
                    if reloadCodeRequested = 4 then // NOTE: takes multiple frames to see dialog.
                        tryReloadCode ()
                        reloadCodeRequested <- 0

                // reloading assets and code dialog
                if reloadAllRequested > 0 then
                    let title = "Reloading assets and code..."
                    if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
                    if ImGui.BeginPopupModal title then
                        ImGui.Text "Gaia is processing your request. Please wait for processing to complete."
                        ImGui.EndPopup ()
                    reloadAllRequested <- inc reloadAllRequested
                    if reloadAllRequested = 4 then // NOTE: takes multiple frames to see dialog.
                        tryReloadAll ()
                        reloadAllRequested <- 0

                // fin
                world

            // propagate exception to dialog
            with exn ->
                let worldOld = World.shelveNonCurrent worldOld
                recoverableExceptionOpt <- Some (exn, worldOld)
                world

        // exception handling dialog
        | Some (exn, worldOld) ->
            let title = "Unhandled Exception!"
            if not (ImGui.IsPopupOpen title) then ImGui.OpenPopup title
            if ImGui.BeginPopupModal title then
                ImGui.Text "Exception text:"
                ImGui.TextWrapped (scstring exn)
                ImGui.Text "How would you like to handle this exception?"
                if ImGui.Button "Ignore exception and revert to old world." then
                    world <- World.unshelve worldOld
                    recoverableExceptionOpt <- None
                if ImGui.Button "Ignore exception and proceed with current world." then
                    recoverableExceptionOpt <- None
                if ImGui.Button "Exit the editor." then
                    showConfirmExitDialog <- true
                    recoverableExceptionOpt <- None
                ImGui.EndPopup ()
            world

    let rec private runWithCleanUp gaiaState targetDir_ screen wtemp =
        world <- wtemp
        openProjectFilePath <- gaiaState.ProjectDllPath
        openProjectImperativeExecution <- gaiaState.ProjectImperativeExecution
        if  not (String.IsNullOrWhiteSpace gaiaState.ProjectDllPath) &&
            not gaiaState.ProjectFreshlyLoaded then
            desiredEyeCenter2d <- gaiaState.DesiredEyeCenter2d
            desiredEyeCenter3d <- gaiaState.DesiredEyeCenter3d
            desiredEyeRotation3d <- gaiaState.DesiredEyeRotation3d
            world <- World.setEyeCenter2d desiredEyeCenter2d world
            world <- World.setEyeCenter3d desiredEyeCenter3d world
            world <- World.setEyeRotation3d desiredEyeRotation3d world
            world <- World.setMasterSoundVolume gaiaState.MasterSoundVolume world
            world <- World.setMasterSongVolume gaiaState.MasterSongVolume world
        targetDir <- targetDir_
        projectDllPath <- openProjectFilePath
        projectFileDialogState <- ImGuiFileDialogState targetDir
        projectEditMode <- Option.defaultValue "" gaiaState.ProjectEditModeOpt
        projectImperativeExecution <- openProjectImperativeExecution
        groupFileDialogState <- ImGuiFileDialogState (targetDir + "/../../..")
        selectScreen screen
        selectGroupInitial screen
        newEntityDispatcherName <- Nu.World.getEntityDispatchers world |> Seq.head |> fun kvp -> kvp.Key
        assetGraphStr <-
            match AssetGraph.tryMakeFromFile (targetDir + "/" + Assets.Global.AssetGraphFilePath) with
            | Right assetGraph ->
                let packageDescriptorsStr = scstring (AssetGraph.getPackageDescriptors assetGraph)
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<AssetGraph>).PrettyPrinter
                PrettyPrinter.prettyPrint packageDescriptorsStr prettyPrinter
            | Left error ->
                messageBoxOpt <- Some ("Could not read asset graph due to: " + error + "'.")
                ""
        overlayerStr <-
            let overlayerFilePath = targetDir + "/" + Assets.Global.OverlayerFilePath
            match Overlayer.tryMakeFromFile [] overlayerFilePath with
            | Right overlayer ->
                let extrinsicOverlaysStr = scstring (Overlayer.getExtrinsicOverlays overlayer)
                let prettyPrinter = (SyntaxAttribute.defaultValue typeof<Overlay>).PrettyPrinter
                PrettyPrinter.prettyPrint extrinsicOverlaysStr prettyPrinter
            | Left error ->
                messageBoxOpt <- Some ("Could not read overlayer due to: " + error + "'.")
                ""
        let result = World.runWithCleanUp tautology id id id imGuiProcess Live true world
        world <- Unchecked.defaultof<_>
        result

    (* Public API *)

    /// Run Gaia.
    let run nuConfig gaiaPlugin =

        // discover the desired nu plugin for editing
        let (gaiaState, targetDir, plugin) = selectNuPlugin gaiaPlugin

        // ensure imgui ini file exists and was created by Gaia before initialising imgui
        let imguiIniFilePath = targetDir + "/imgui.ini"
        if  not (File.Exists imguiIniFilePath) ||
            (File.ReadAllLines imguiIniFilePath).[0] <> "[Window][Gaia]" then
            File.WriteAllText (imguiIniFilePath, ImGuiIniFileStr)

        // attempt to create SDL dependencies
        match tryMakeSdlDeps () with
        | Right (sdlConfig, sdlDeps) ->

            // attempt to create the world
            let worldConfig =
                { Imperative = gaiaState.ProjectImperativeExecution
                  Advancing = false
                  ModeOpt = gaiaState.ProjectEditModeOpt
                  NuConfig = nuConfig
                  SdlConfig = sdlConfig }
            match tryMakeWorld sdlDeps worldConfig plugin with
            | Right (screen, world) ->

                // subscribe to events related to editing
                let world = World.subscribe handleNuMouseButton Game.MouseLeftDownEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseLeftUpEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseMiddleDownEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseMiddleUpEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseRightDownEvent Game world
                let world = World.subscribe handleNuMouseButton Game.MouseRightUpEvent Game world
                let world = World.subscribe handleNuSelectedScreenOptChange Game.SelectedScreenOpt.ChangeEvent Game world
                let world = World.subscribe handleNuRender Game.RenderEvent Game world
                
                // run the world
                runWithCleanUp gaiaState targetDir screen world
            | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure
        | Left error -> Log.trace error; Constants.Engine.ExitCodeFailure