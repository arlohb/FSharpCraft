module Lib

/// Flatten an Array3D into a flat array.
let flattenArray3D (array: 'a array3d) =
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
