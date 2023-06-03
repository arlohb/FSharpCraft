open FSharp.Collections
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.UI
open Aardvark.UI.Primitives
open FShade
open Suave

// [<EntryPoint; STAThread>]
[<EntryPoint>]
let main args =
    match args with
    | [||] ->
        Aardvark.Init()

        let win =
            window {
                backend Backend.GL
                display Display.Mono
                debug false
                samples 4
            }

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

        let world = new World.World(Biome.getWorldGen)

        let chunkGenAgent =
            Agents.simpleAgent (fun chunkId ->
                if world.Chunks.ContainsKey chunkId |> not then
                    printf "+"
                    world.CreateChunk chunkId
                    meshGenAgent.Post(world, chunkId))

        let texture = new FileTexture("assets/Texture.png", false) |> AVal.constant
        let texture = NoiseDemo.texture win.Time

        printfn "Generating chunks"

        // Generate chunks
        Camera.chunksAroundCamera V3d.Zero 10 -2 2
        |> Array.iter (fun chunkId ->
            world.CreateChunk chunkId
            meshGenAgent.Post(world, chunkId))

        printfn "Chunks generated"

        meshGenStart.Trigger()

        printfn "Mesh agent started"

        use _ =
            view.AddCallback(fun (view: CameraView) ->
                Camera.chunksAroundCamera view.Location 16 -2 2
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
                      DefaultSemantic.ColorTexture, typeof<Symbol>
                      // DefaultSemantic.DiffuseColorTexture, typeof<Symbol>
                      DefaultSemantic.DiffuseColorCoordinates, typeof<V2f> ])
            |> Sg.texture Mesh.blockTexture texture
            // |> Sg.diffuseTexture texture
            |> Sg.uniform "myUniform" texture
            |> Sg.shader {
                do! DefaultSurfaces.trafo

                // do! DefaultSurfaces.diffuseTexture
                do!
                    let diffuseSampler =
                        sampler2d {
                            texture uniform?myUniform
                            filter Filter.MinMagPoint
                            addressU WrapMode.Wrap
                            addressV WrapMode.Wrap
                        }

                    (fun (v: Effects.Vertex) ->
                        fragment {
                            let texColour = diffuseSampler.Sample(v.tc)
                            return texColour
                        })

                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.viewTrafo (view |> AVal.map CameraView.viewTrafo)
            |> Sg.projTrafo (
                win.Sizes
                |> AVal.map (fun s ->
                    Frustum.perspective 60.0 0.1 10000.0 (float s.X / float s.Y)
                    |> Frustum.projTrafo)
            )

        win.Scene <- sg
        win.Run()
        0
    | [| "bench" |] ->
        Bench.run ()
        0
    | [| "gui" |] ->
        Aardvark.Init()
        // Aardium.init ()

        // let win =
        //     window {
        //         backend Backend.GL
        //         display Display.Mono
        //         debug false
        //         samples 4
        //     }

        let app = new Aardvark.Application.Slim.OpenGlApplication()

        WebPart.startServerLocalhost 4321 [ MutableApp.toWebPart' app.Runtime false (App.start App.app) ]
        |> ignore

        while true do
            ()

        // Aardium.run {
        //     title "Aardvark rocks \\o/"
        //     width 1024
        //     height 768
        //     // url "http://localhost:4321/"
        //     url "https://www.google.com"
        // }

        0
    | _ ->
        printfn "Args not recognised"
        0

// https://github.com/aardvark-platform/aardvark.rendering/tree/master/src/Examples%20(netcore)
// https://github.com/aardvark-platform/aardvark.docs/wiki/Gallery
// https://github.com/aardvark-platform/aardvark.docs/wiki
// https://github.com/aardvark-platform/aardvark.docs/wiki/Hello-World-Tutorial
