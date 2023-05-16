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
/// Only checks for positive edge
let isOutside (p: V3i) =
    p.X >= Size || p.Y >= Size || p.Z >= Size || p.X < 0 || p.Y < 0 || p.Z < 0

let inRange xRange yRange zRange =
    xRange
    |> Array.allPairs yRange
    |> Array.allPairs zRange
    |> Array.map (fun (z, (y, x)) -> (x, y, z))
