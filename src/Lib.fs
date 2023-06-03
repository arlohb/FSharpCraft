module Lib

open Aardvark.Base

/// Flatten an Array3D into a flat array.
let flattenArray3D (array: 'a[,,]) =
    [| 0 .. Array3D.length1 array - 1 |]
    |> Array.map (fun x -> ([| 0 .. Array3D.length2 array - 1 |] |> Array.map (fun y -> array[x, y, *])))
    |> Array.reduce Array.append
    |> Array.reduce Array.append

/// Zip an array with each element ran through a function.
let zipMap f array = array |> Array.map (fun x -> (x, f x))

/// Zip an array with each element ran through a function.
/// Then flatten the result of the function.
let zipMapFlat f array =
    array
    |> zipMap f
    |> Array.collect (fun (a, b) -> b |> Array.zip (Array.init b.Length (fun _ -> a)))

/// Return an array of tuples for each element in each range.
let inRange xRange yRange zRange =
    xRange
    |> Array.allPairs yRange
    |> Array.allPairs zRange
    |> Array.map (fun (z, (y, x)) -> (x, y, z))

/// Create an array of numbers around the center
let range radius center =
    [| center - radius .. center + radius |]

let aroundPointXY xyRadius zMin zMax (point: V3i) =
    inRange (range xyRadius point.X) (range xyRadius point.Y) [| zMin..zMax |]

let aroundPoint radius (point: V3i) =
    inRange (range radius point.X) (range radius point.Y) (range radius point.Z)
