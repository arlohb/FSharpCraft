// Used when world and meshGenAgent are created.
// The compiler can't verify the references to each other aren't
// used before the other is initialised.
// I can verify this, so disable this warning.
#nowarn "40"

open System.Threading.Tasks
open FSharp.Collections
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim

open Camera
open World

// [<EntryPoint; STAThread>]
[<EntryPoint>]
let main _ =
    Aardvark.Init()

    use app = new OpenGlApplication()
    use win = app.CreateGameWindow(4)

    let view =
        camera win.Keyboard win.Mouse win.Time (CameraView.lookAt (V3d(6, 6, 6)) V3d.Zero V3d.OOI)

    let meshes: cmap<Chunk.Id, Mesh.Mesh> = cmap ()

    let activateMeshGenAgent = new Event<unit>()

    let rec world = new World(Biome.getWorldGen, meshGenAgent.Post)

    and meshAddAgent: MailboxProcessor<Chunk.Id * Mesh.Mesh> =
        MailboxProcessor<_>.Start(fun inbox ->
            let rec messageLoop () =
                async {
                    let! chunkId, mesh = inbox.Receive()

                    transact (fun () -> meshes.Add(chunkId, mesh) |> ignore)

                    return! messageLoop ()
                }

            messageLoop ())

    and meshGenAgent: MailboxProcessor<Chunk.Id> =
        MailboxProcessor<Chunk.Id>.Start(fun inbox ->
            activateMeshGenAgent.Publish |> Async.AwaitEvent |> Async.RunSynchronously

            let rec messageLoop tasks =
                async {
                    let! chunkId = inbox.TryReceive 100

                    let tasks =
                        tasks
                        |> List.append (
                            match chunkId with
                            | Some chunkId ->
                                // Create and start the task
                                [ Task.Run(fun () ->
                                      let mesh = world.CreateMesh chunkId
                                      meshAddAgent.Post(chunkId, mesh)
                                      printf "_") ]
                            | None -> []
                        )

                    // Partition into complete and still running
                    let complete, tasks = tasks |> List.partition (fun t -> t.IsCompleted)

                    // Delete all the completed tasks
                    complete |> List.iter (fun t -> t.Dispose())

                    return! messageLoop tasks
                }

            messageLoop [])

    let chunkGenAgent =
        MailboxProcessor<Chunk.Id>.Start(fun inbox ->
            let rec messageLoop () =
                async {
                    let! chunkId = inbox.Receive()

                    if world.Chunks.ContainsKey chunkId |> not then
                        printf "+"
                        world.CreateChunk chunkId true
                    else
                        ()

                    return! messageLoop ()
                }

            messageLoop ())

    let texture = new FileTexture("assets/Texture.png", false)

    let chunkRadius = 10
    let chunkZRadius = 2

    printfn "Generating chunks"

    // Generate chunks
    [| -chunkRadius .. chunkRadius |]
    |> Array.allPairs [| -chunkRadius .. chunkRadius |]
    |> Array.allPairs [| -chunkZRadius .. chunkZRadius |]
    |> Array.map (fun (z, (y, x)) -> V3i(x, y, z) |> Chunk.Id)
    |> Array.iter (fun chunkId -> world.CreateChunk chunkId false)

    printfn "Chunks generated"

    activateMeshGenAgent.Trigger()

    printfn "Mesh agent started"

    use _ =
        view.AddCallback(fun (view: CameraView) ->
            chunksAroundCamera view.Location 4 -1 2
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
