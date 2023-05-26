module World

open FSharp.Data.Adaptive
open Aardvark.Base

type World(blockGen, meshAgentPost) =
    member val Chunks: cmap<Chunk.Id, Chunk.Chunk> = cmap ()
    member val BlockGenerator: Biome.Biome -> int -> int -> int -> Block.Block = blockGen
    member val MeshAgentPost: World * Chunk.Id -> unit = meshAgentPost

    member this.CreateChunk spreadEvent chunkId =
        let newChunk = Chunk.create this.BlockGenerator chunkId

        this.Chunks.Add(chunkId, newChunk) |> ignore

        if spreadEvent then
            Chunk.adjacentChunks chunkId
            |> Array.filter this.Chunks.ContainsKey
            |> Array.iter (fun chunkId -> meshAgentPost (this, chunkId))
        else
            meshAgentPost (this, chunkId)

    member this.CreateMesh(Chunk.Id chunkId) =
        /// Get the face at this position in this direction
        /// Returns None if there is no face
        let getFace chunkId pos dir =
            let chunk = this.Chunks[chunkId]
            let block1 = chunk.GetBlock pos

            // Get the next block along the direction
            let pos' = pos + dir

            match pos' with
            // If this is outside the chunk
            | Chunk.IsOutside pos' ->
                // Get the chunk it would be in
                let chunkPos' = chunkId.value + dir |> Chunk.Id

                match this.Chunks.ContainsKey(chunkPos'), block1 with
                // If this chunk exists
                | true, _ ->
                    // Read from it
                    let chunk = this.Chunks[chunkPos']
                    // And fix pos
                    // The addition is to fix negatives
                    let pos' = (pos' + Chunk.Size) % Chunk.Size

                    let block2 = chunk.GetBlock pos'
                    Block.getFaceBetween block1 block2 |> Option.map (fun b -> (dir, b))

                // If this block is air
                | _, Block.Air -> None

                // If this block is a block, create a face
                | _, _ -> Some(dir, block1)
            | _ ->
                let block2 = chunk.GetBlock pos'
                Block.getFaceBetween block1 block2 |> Option.map (fun b -> (dir, b))

        /// Get all the faces of a block in a chunk
        let getFaces chunkId (pos: V3i) =
            if pos.X = 0 || pos.Y = 0 || pos.Z = 0 then
                Block.pDirections
            else
                Block.pDirections
            |> Array.map (getFace chunkId pos)
            |> Array.filter Option.isSome
            |> Array.map Option.get

        this.Chunks[Chunk.Id chunkId].Blocks
        |> Array3D.mapi (fun x y z _ -> V3i(x, y, z))
        |> Lib.flattenArray3D
        |> Lib.zipMapFlat (getFaces <| Chunk.Id chunkId)
        |> Array.map (fun (pos, face) -> Block.createFace (Chunk.Size * chunkId + pos) face)
        |> function
            | [||] -> Mesh.empty
            | arr -> Array.reduce Mesh.merge arr
