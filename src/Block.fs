module Block

open Aardvark.Base

let pDirections = [| V3i.IOO; V3i.OIO; V3i.OOI |]
let nDirections = [| V3i.NOO; V3i.ONO; V3i.OON |]
let directions = Array.append pDirections nDirections

type Block =
    | Air
    | Dirt
    | Stone

let isAir block =
    match block with
    | Air -> true
    | _ -> false

let colour block =
    match block with
    | Air -> C4b(0, 0, 0, 0)
    | Dirt -> C4b(0.3, 0.2, 0)
    | Stone -> C4b(0.5)

let uv block =
    let (x, y) =
        match block with
        | Air -> (0, 0)
        | Dirt -> (2, 15)
        | Stone -> (3, 15)

    V2f(x, y) / 16f

// Without this extra value
// There are lines between blocks
let private epsilon = pow 10f -2f

/// Get the UVs of a face.
/// This is on the block spritesheet.
let faceUvs' =
    [| V2f.OO + epsilon * V2f.II
       V2f.OI / 16f + epsilon * V2f.PN
       V2f.II / 16f + epsilon * V2f.NN
       V2f.IO / 16f + epsilon * V2f.NP |]

let faceUvs block =
    let uvOffset = uv block
    faceUvs' |> Array.map ((+) uvOffset)

let createFace (pos: V3i) (dir: V3i, block) =
    let points =
        match (dir.X, dir.Y, dir.Z) with
        | (1, 0, 0) -> [| V3f.IOO; V3f.IOI; V3f.III; V3f.IIO |]
        | (0, 1, 0) -> [| V3f.IIO; V3f.III; V3f.OII; V3f.OIO |]
        | (0, 0, 1) -> [| V3f.OOI; V3f.OII; V3f.III; V3f.IOI |]
        | (-1, 0, 0) -> [| V3f.OIO; V3f.OII; V3f.OOI; V3f.OOO |]
        | (0, -1, 0) -> [| V3f.OOO; V3f.OOI; V3f.IOI; V3f.IOO |]
        | (0, 0, -1) -> [| V3f.OOO; V3f.OIO; V3f.IIO; V3f.IOO |]
        | _ -> invalidArg "dir" "Needs to be a unit vector in a cardinal direction"

    Mesh.create
        (points
         |> Array.zip (faceUvs block)
         |> Array.map (fun (uv, p) ->
             { Position = V3f(pos) + p
               Normal = dir.ToV3f()
               Uv = uv }))
        [| 0; 1; 2; 0; 2; 3 |]


/// Get the block of a face between these two blocks
/// If one exists
let getFaceBetween block1 block2 =
    match isAir block1, isAir block2 with
    | true, true
    | false, false -> None
    | true, false -> Some block2
    | false, true -> Some block1
