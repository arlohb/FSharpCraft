open FSharp.Collections
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim

// [<EntryPoint; STAThread>]
[<EntryPoint>]
let main _ =
    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(4)

    let view =
        Camera.camera win.Keyboard win.Mouse win.Time (CameraView.lookAt (V3d(6, 6, 6)) V3d.Zero V3d.OOI)

    let meshes: cmap<Chunk.Id, Mesh.Mesh> = cmap ()

    let meshAddAgent =
        Agents.simpleAgent (fun (chunkId, mesh) -> transact (fun () -> meshes.Add(chunkId, mesh) |> ignore))

    let meshGenStart, meshGenAgent =
        Agents.taskPoolAgent (fun (world: World.World, chunkId) ->
            let mesh = world.CreateMesh chunkId
            meshAddAgent.Post(chunkId, mesh)
            printf "_")

    let world = new World.World(Biome.getWorldGen, meshGenAgent.Post)

    let chunkGenAgent =
        Agents.simpleAgent (fun chunkId ->
            if world.Chunks.ContainsKey chunkId |> not then
                printf "+"
                world.CreateChunk chunkId true
            else
                ())

    let texture = new FileTexture("assets/Texture.png", false)

    printfn "Generating chunks"

    // Generate chunks
    Camera.chunksAroundCamera V3d.Zero 10 -2 2
    |> Array.iter (fun chunkId -> world.CreateChunk chunkId false)

    printfn "Chunks generated"

    meshGenStart.Trigger()

    printfn "Mesh agent started"

    use _ =
        view.AddCallback(fun (view: CameraView) ->
            Camera.chunksAroundCamera view.Location 4 -1 2
            |> Array.filter (world.Chunks.ContainsKey >> not)
            |> Array.iter chunkGenAgent.Post)

    let sg =
        meshes
        |> AMap.toASet
        |> ASet.map (snd >> Mesh.createGeometry)
        |> Sg.geometrySet
            IndexedGeometryMode.TriangleList
            (Map.ofList
                [ DefaultSemantic.Positions, typeof<V3f>
                  DefaultSemantic.Normals, typeof<V3f>
                  DefaultSemantic.DiffuseColorCoordinates, typeof<V2f> ])
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
        |> Sg.projTrafo (
            win.Sizes
            |> AVal.map (fun s ->
                Frustum.perspective 60.0 0.1 10000.0 (float s.X / float s.Y)
                |> Frustum.projTrafo)
        )

    let task = app.Runtime.CompileRender(win.FramebufferSignature, sg)

    win.RenderTask <- task
    win.Run()
    0

// https://github.com/aardvark-platform/aardvark.rendering/tree/master/src/Examples%20(netcore)
// https://github.com/aardvark-platform/aardvark.docs/wiki/Gallery
// https://github.com/aardvark-platform/aardvark.docs/wiki
// https://github.com/aardvark-platform/aardvark.docs/wiki/Hello-World-Tutorial
