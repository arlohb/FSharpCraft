open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open FSharp.Data.Adaptive

type Mesh =
    { Points: (V3f * C4b) array
      Triangles: int array }

let indexToAngle indexCount index =
    2. * Math.PI * (float index) / (float indexCount)

let angleToPoints radius angle = radius * V3f(sin angle, cos angle, 0)

let circle radius vertexCount colour =
    let indices = [ 0 .. vertexCount - 1 ]

    let points =
        indices
        // Indices -> Angles -> Points
        |> List.map ((indexToAngle vertexCount) >> (angleToPoints radius))
        // Add the origin
        |> List.append [ V3f.Zero ]
        // Points with colours
        |> List.map (fun p -> (p, colour p))
        |> List.toArray

    let triangles =
        indices
        // Offset all the indices
        |> List.map ((+) 1)
        // Add the origin index
        |> List.append [ 0 ]
        |> List.pairwise
        |> List.skip 1
        |> List.map (fun (a, b) -> [ 0; a; b ])
        |> List.append [ [ 0; vertexCount; 1 ] ]
        |> List.collect id
        |> List.toArray

    { Points = points
      Triangles = triangles }

let createGeometry mesh =
    IndexedGeometry(
        Mode = IndexedGeometryMode.TriangleList,
        IndexArray = mesh.Triangles,
        IndexedAttributes =
            SymDict.ofList
                [ DefaultSemantic.Positions, mesh.Points |> Array.map (fun (p, _) -> p) :> Array
                  DefaultSemantic.Colors, mesh.Points |> Array.map (fun (_, c) -> c) :> Array ]
    )

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

    let colour (p: V3f) = C4b(p.X, p.Y, -0.5f * (p.X + p.Y))

    let sg =
        [ createGeometry (circle 2f 128 colour) ]
        |> List.map Sg.ofIndexedGeometry
        // |> List.append [Sg.sphere 1 (C4b.Cyan |> AVal.init) (1.0 |> AVal.init)]
        |> Sg.ofList
        |> Sg.effect [ DefaultSurfaces.trafo |> toEffect; DefaultSurfaces.vertexColor |> toEffect ]
        |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
        |> Sg.projTrafo (proj |> AVal.map Frustum.projTrafo)

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)

    win.RenderTask <- task
    win.Run()
    0
