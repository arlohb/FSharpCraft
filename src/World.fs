module World

open FSharp.Data.Adaptive

type World(blockGen) =
    member val Chunks: cmap<Chunk.Id, Chunk.Chunk> = cmap ()
    member val BlockGenerator: Biome.Biome -> int -> int -> int -> Block.Block = blockGen

    member this.CreateChunk chunkId =
        let newChunk = Chunk.create this.BlockGenerator chunkId
        this.Chunks.Add(chunkId, newChunk) |> ignore

    member this.CreateMesh chunkId =
        this.Chunks[chunkId] |> Chunk.createMesh
