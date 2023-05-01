open System
open FSharp.Collections
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim

open Mesh
open World

[<EntryPoint; STAThread>]
let main _ =
    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(4)

    let initialView = CameraView.lookAt (V3d(6, 6, 6)) V3d.Zero V3d.OOI

    let view =
        AVal.integrate
            initialView
            win.Time
            [ (DefaultCameraController.controlWSAD win.Keyboard win.Time)
              (DefaultCameraController.controlLookAround win.Mouse)
              (DefaultCameraController.controlPan win.Mouse)
              (DefaultCameraController.controlZoom win.Mouse)
              (DefaultCameraController.controllScroll win.Mouse win.Time) ]

    let proj =
        win.Sizes
        |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

    let world = defaultWorld

    let texture = new FileTexture("assets/Texture.png", false)

    let chunkRadius = 10
    let chunkZRadius = 3

    [| -chunkRadius .. chunkRadius |]
    |> Array.allPairs [| -chunkRadius .. chunkRadius |]
    |> Array.allPairs [| -chunkZRadius .. chunkZRadius |]
    |> Array.iter (fun (z, (y, x)) -> world.CreateChunk x y z)

    let meshes =
        [| -chunkRadius .. chunkRadius |]
        |> Array.allPairs [| -chunkRadius .. chunkRadius |]
        |> Array.allPairs [| -chunkZRadius .. chunkZRadius |]
        |> Array.map (fun (z, (y, x)) -> world.CreateMesh(x, y, z))
        |> Array.map createGeometry
        |> ASet.ofArray

    let sg =
        Sg.geometrySet
            IndexedGeometryMode.TriangleList
            (Map.ofList
                [ DefaultSemantic.Positions, typeof<V3f>
                  DefaultSemantic.Normals, typeof<V3f>
                  DefaultSemantic.DiffuseColorCoordinates, typeof<V2f> ])
            meshes
        |> Sg.scale 0.1
        |> Sg.diffuseTexture (texture |> AVal.constant)
        |> Sg.samplerState
            DefaultSemantic.DiffuseColorTexture
            (SamplerState.Default
             |> SamplerState.withFilter TextureFilter.MinMagPoint
             |> Some
             |> AVal.constant)
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.diffuseTexture
            do! DefaultSurfaces.simpleLighting
        }
        |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
        |> Sg.projTrafo (proj |> AVal.map Frustum.projTrafo)

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)

    win.RenderTask <- task
    win.Run()
    0

// https://github.com/aardvark-platform/aardvark.rendering/tree/master/src/Examples%20(netcore)
// https://github.com/aardvark-platform/aardvark.docs/wiki/Gallery
// https://github.com/aardvark-platform/aardvark.docs/wiki
// https://github.com/aardvark-platform/aardvark.docs/wiki/Hello-World-Tutorial
