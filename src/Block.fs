module Block

open Aardvark.Base

type Direction =
    | Px
    | Py
    | Pz
    | Nx
    | Ny
    | Nz

let directions = [| Px; Py; Pz; Nx; Ny; Nz |]
let pDirections = [| Px; Py; Pz |]

let addDirection (x, y, z) dir =
    match dir with
    | Px -> (x + 1, y, z)
    | Py -> (x, y + 1, z)
    | Pz -> (x, y, z + 1)
    | Nx -> (x - 1, y, z)
    | Ny -> (x, y - 1, z)
    | Nz -> (x, y, z - 1)

type Block =
    | Air
    | Dirt
    | Stone

let blockColour block =
    match block with
    | Air -> C4b(0, 0, 0, 0)
    | Dirt -> C4b(0.3, 0.2, 0)
    | Stone -> C4b(0.5)

let blockUv _block = V2f.Zero

let isAir block =
    match block with
    | Air -> true
    | _ -> false
