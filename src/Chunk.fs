module Chunk

open Aardvark.Base

open Block

[<Literal>]
let ChunkSize = 16

type Chunk = { Position: V3f; Blocks: Block[,,] }

let createChunk blockGen position =
    { Position = position
      Blocks =
        Array3D.init ChunkSize ChunkSize ChunkSize (fun x y z ->
            blockGen (x + int position.X) (y + int position.Y) (z + int position.Z)) }
