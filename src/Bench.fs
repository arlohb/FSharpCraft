module Bench

open Aardvark.Base

let bench title range fn =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    range |> List.iter (fun i -> fn i |> ignore)

    stopwatch.Stop()

    let total = stopwatch.Elapsed.TotalMilliseconds
    let average = total / float range.Length

    printfn "%-15s: avg %f ms over %d iters" title average range.Length

let run () =
    let chunkGenRange =
        Lib.aroundPoint 5 V3i.Zero |> Array.toList |> List.map (V3i >> Chunk.Id)

    bench "Chunk Gen" chunkGenRange (Chunk.create Biome.getWorldGen)

    let world = new World.World(Biome.getWorldGen, ignore)

    chunkGenRange |> List.iter (fun chunkId -> world.CreateChunk false chunkId)

    bench "Mesh Gen" chunkGenRange world.CreateMesh
