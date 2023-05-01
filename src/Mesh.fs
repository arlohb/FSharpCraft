module Mesh

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering

type Vertex = { Position: V3f; Normal: V3f; Uv: V2f }

type Mesh =
    { Vertices: Vertex array
      Triangles: int array }

    static member Empty = { Vertices = [||]; Triangles = [||] }

let mergeMesh mesh1 mesh2 =
    { Vertices = Array.append mesh1.Vertices mesh2.Vertices
      Triangles = Array.append mesh1.Triangles (mesh2.Triangles |> Array.map ((+) mesh1.Vertices.Length)) }

let createGeometry mesh =
    IndexedGeometry(
        Mode = IndexedGeometryMode.TriangleList,
        IndexArray = mesh.Triangles,
        IndexedAttributes =
            SymDict.ofList
                [ DefaultSemantic.Positions, mesh.Vertices |> Array.map (fun p -> p.Position) :> Array
                  DefaultSemantic.Normals, mesh.Vertices |> Array.map (fun p -> p.Normal) :> Array
                  DefaultSemantic.DiffuseColorCoordinates, mesh.Vertices |> Array.map (fun p -> p.Uv) :> Array ]
    )
