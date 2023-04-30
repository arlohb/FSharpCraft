module World

open FSharp.Data.Adaptive
open Aardvark.Base
open DotnetNoise

open Block
open Chunk
open Mesh

type World =
    { mutable Chunks: HashMap<(int * int * int), Chunk>
      BlockGenerator: int -> int -> int -> Block }

    member this.CreateChunk x y z =
        let newChunk =
            createChunk this.BlockGenerator (16f * V3f(float x, float y, float z))

        this.Chunks <- this.Chunks.Add((x, y, z), newChunk)

    member this.CreateMesh(chunkPos: (int * int * int)) =
        /// Whether this block is outside the chunk
        /// Only checks for positive edge
        let outsideChunk (x, y, z) =
            x >= ChunkSize || y >= ChunkSize || z >= ChunkSize

        /// Get the block of a face between these two blocks
        /// If one exists
        let getFaceBetweenBlocks block1 block2 =
            if isAir block1 = isAir block2 then None
            elif isAir block1 then Some(block2)
            else Some(block1)

        /// Get the face at this position in this direction
        /// Returns None if there is no face
        let getFace chunkPos x y z dir =
            let chunk = this.Chunks[chunkPos]
            let block1 = chunk.Blocks[x, y, z]

            // Get the next block along the direction
            let (x', y', z') = addDirection (x, y, z) dir

            // If this is outside the chunk
            if outsideChunk (x', y', z') then
                // Get the chunk it would be in
                let chunkPos' = addDirection chunkPos dir

                // If this chunk exists
                if this.Chunks.ContainsKey(chunkPos') then
                    // Read from it
                    let chunk = this.Chunks[chunkPos']
                    // And fix x' y' z'
                    let x' = x' % ChunkSize
                    let y' = y' % ChunkSize
                    let z' = z' % ChunkSize

                    let block2 = chunk.Blocks[x', y', z']
                    getFaceBetweenBlocks block1 block2 |> Option.map (fun b -> (dir, b))
                else if
                    // If the chunk isn't generated,
                    // just make a face if not air
                    isAir block1
                then
                    None
                else
                    Some((dir, block1))
            else
                let block2 = chunk.Blocks[x', y', z']
                getFaceBetweenBlocks block1 block2 |> Option.map (fun b -> (dir, b))

        /// Get all the faces of a block in a chunk
        let getFaces chunkPos (x, y, z) =
            pDirections
            |> Array.map (getFace chunkPos x y z)
            |> Array.filter Option.isSome
            |> Array.map Option.get

        let buildFace (x, y, z) (direction, block) =
            let (points, normal) =
                match direction with
                | Px -> ([| V3f.IOO; V3f.IOI; V3f.III; V3f.IIO |], V3f.IOO)
                | Py -> ([| V3f.IIO; V3f.III; V3f.OII; V3f.OIO |], V3f.OIO)
                | Pz -> ([| V3f.OOI; V3f.OII; V3f.III; V3f.IOI |], V3f.OOI)
                | Nx -> ([| V3f.OIO; V3f.OII; V3f.OOI; V3f.OOO |], -V3f.IOO)
                | Ny -> ([| V3f.OOO; V3f.OOI; V3f.IOI; V3f.IOO |], -V3f.OIO)
                | Nz -> ([| V3f.OOO; V3f.OIO; V3f.IIO; V3f.IOO |], -V3f.OOI)

            let uvs = [| V2f.OO; V2f.OI; V2f.II; V2f.OI |]

            { Vertices =
                points
                |> Array.zip uvs
                |> Array.map (fun (uv, p) ->
                    let cx, cy, cz = chunkPos

                    { Position = 16f * V3f(cx, cy, cz) + p + V3f(float x, float y, float z)
                      Normal = normal
                      Uv = uv + blockUv block })
              Triangles = [| 0; 1; 2; 0; 2; 3 |] }

        let flattenArray (array: 'a array3d) =
            [| 0 .. Array3D.length1 array - 1 |]
            |> Array.map (fun x -> ([| 0 .. Array3D.length2 array - 1 |] |> Array.map (fun y -> array[x, y, *])))
            |> Array.reduce Array.append
            |> Array.reduce Array.append

        let zipMap fn array = array |> Array.map (fun x -> (x, fn x))

        this.Chunks[chunkPos].Blocks
        |> Array3D.mapi (fun x y z _ -> (x, y, z))
        |> flattenArray
        |> zipMap (getFaces chunkPos)
        |> Array.map (fun (pos, faces) -> faces |> Array.map (buildFace pos))
        |> Array.collect id
        |> Array.reduce mergeMesh

let noise = new FastNoise()

let defaultWorld =
    { Chunks = HashMap.empty
      BlockGenerator =
        (fun x y z ->
            let height = noise.GetSimplexFractal(float32 x, float32 y) * 5f + 8f

            if float32 z < height - 3f then Stone
            elif float32 z <= height then Dirt
            else Air) }
