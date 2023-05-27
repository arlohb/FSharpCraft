module Chunk

open Aardvark.Base

[<Literal>]
let Size = 16

type Id =
    | Id of V3i

    member this.toWorldPos() =
        let (Id pos) = this
        Size * pos

    member this.value =
        let (Id value) = this
        value

type Chunk =
    { Position: V3i
      Blocks: Block.Block[,,]
      Biomes: Biome.Biome[,] }

    member this.GetBlock(pos: V3i) =
        let (x, y, z) = (pos.X, pos.Y, pos.Z)
        this.Blocks[x, y, z]

let create blockGen (chunkId: Id) =
    let pos = chunkId.toWorldPos ()

    let biomes = Array2D.init Size Size (fun x y -> Biome.get (x + pos.X) (y + pos.Y))

    { Position = pos
      Blocks = Array3D.init Size Size Size (fun x y z -> blockGen biomes[x, y] (x + pos.X) (y + pos.Y) (z + pos.Z))
      Biomes = biomes }

/// Whether this block is outside the chunk
let isOutside (p: V3i) =
    p.X >= Size || p.Y >= Size || p.Z >= Size || p.X < 0 || p.Y < 0 || p.Z < 0

let (|IsOutside|_|) pos =
    match isOutside pos with
    | true -> Some pos
    | false -> None

let createMesh (chunk: Chunk) =
    /// Get the face at this position in this direction
    /// Returns None if there is no face
    let getFace pos dir =
        let block1 = chunk.GetBlock pos

        // Get the next block along the direction
        let pos' = pos + dir

        match pos', block1 with
        // If this is outside the chunk
        // And air
        | IsOutside _, Block.Air -> None
        // If this is outside the chunk
        // And a solid block
        | IsOutside _, _ -> Some(dir, block1)
        | _ ->
            let block2 = chunk.GetBlock pos'
            Block.getFaceBetween block1 block2 |> Option.map (fun b -> (dir, b))

    /// Get all the faces of a block in a chunk
    let getFaces (pos: V3i) =
        if pos.X = 0 || pos.Y = 0 || pos.Z = 0 then
            Block.pDirections
        else
            Block.pDirections
        |> Array.map (getFace pos)
        |> Array.filter Option.isSome
        |> Array.map Option.get

    chunk.Blocks
    |> Array3D.mapi (fun x y z _ -> V3i(x, y, z))
    |> Lib.flattenArray3D
    |> Lib.zipMapFlat getFaces
    |> Array.map (fun (pos, face) -> Block.createFace (chunk.Position + pos) face)
    |> function
        | [||] -> Mesh.empty
        | arr -> arr |> Array.reduce Mesh.merge
