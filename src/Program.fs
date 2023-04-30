open System
open FSharp.Data.Adaptive
open FSharp.Collections
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
        initialView |> DefaultCameraController.control win.Mouse win.Keyboard win.Time

    let proj =
        win.Sizes
        |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y))

    let world = defaultWorld

    for x = -5 to 5 do
        for y = -5 to 5 do
            world.CreateChunk x y 0

    let sg =
        [| -5 .. 5 |]
        |> Array.allPairs [| -5 .. 5 |]
        |> Array.map (fun (x, y) -> world.CreateMesh(x, y, 0))
        |> Array.map (createGeometry >> Sg.ofIndexedGeometry)
        |> Array.toList
        |> Sg.ofList
        |> Sg.scale 0.04
        |> Sg.diffuseTexture DefaultTextures.checkerboard
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
