module World

open FSharp.Data.Adaptive
open Aardvark.Base

open Lib

type World(blockGen, meshAgentPost) =
    member val Chunks: cmap<Chunk.Id, Chunk.Chunk> = cmap ()
    member val BlockGenerator: Biome.Biome -> int -> int -> int -> Block.Block = blockGen
    member val MeshAgentPost: Chunk.Id -> unit = meshAgentPost

    member this.CreateChunk chunkId spreadEvent =
        let newChunk = Chunk.create this.BlockGenerator chunkId

        this.Chunks.Add(chunkId, newChunk) |> ignore

        let (Chunk.Id chunkPos) = chunkId
        let (x, y, z) = (chunkPos.X, chunkPos.Y, chunkPos.Z)

        if spreadEvent then
            [| x - 1 .. x + 1 |]
            |> Array.allPairs [| y - 1 .. y + 1 |]
            |> Array.allPairs [| z - 1 .. z + 1 |]
            |> Array.map (fun (z, (y, x)) -> V3i(x, y, z) |> Chunk.Id)
            |> Array.filter this.Chunks.ContainsKey
            |> Array.iter meshAgentPost
        else
            meshAgentPost chunkId

    member this.CreateMesh(Chunk.Id chunkId) =
        /// Get the face at this position in this direction
        /// Returns None if there is no face
        let getFace chunkId pos dir =
            let chunk = this.Chunks[chunkId]
            let block1 = chunk.GetBlock pos

            // Get the next block along the direction
            let pos' = pos + dir

            // If this is outside the chunk
            if Chunk.isOutside pos' then
                // Get the chunk it would be in
                let chunkPos' = chunkId.value + dir |> Chunk.Id

                // If this chunk exists
                if this.Chunks.ContainsKey(chunkPos') then
                    // Read from it
                    let chunk = this.Chunks[chunkPos']
                    // And fix pos
                    let pos' = pos' % Chunk.Size

                    let block2 = chunk.GetBlock pos'
                    Block.getFaceBetween block1 block2 |> Option.map (fun b -> (dir, b))
                else if
                    // If the chunk isn't generated,
                    // just make a face if not air
                    Block.isAir block1
                then
                    None
                else
                    Some((dir, block1))
            else
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
        |> flattenArray3D
        |> zipMapFlat (getFaces (Chunk.Id chunkId))
        |> Array.map (fun (pos, face) -> Block.createFace (Chunk.Size * chunkId + pos) face)
        |> function
            | [||] -> Mesh.empty
            | arr -> Array.reduce Mesh.merge arr
