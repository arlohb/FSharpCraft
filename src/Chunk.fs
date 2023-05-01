module Chunk

open Aardvark.Base

open Block
open Biome

[<Literal>]
let ChunkSize = 16

type Chunk =
    { Position: V3f
      Blocks: Block[,,]
      Biomes: Biome[,] }

let createChunk blockGen (position: V3f) =
    let biomes =
        Array2D.init ChunkSize ChunkSize (fun x y -> getBiome (x + int position.X) (y + int position.Y))

    { Position = position
      Blocks =
        Array3D.init ChunkSize ChunkSize ChunkSize (fun x y z ->
            blockGen biomes[x, y] (x + int position.X) (y + int position.Y) (z + int position.Z))
      Biomes = biomes }
