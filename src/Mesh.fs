module Mesh

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph

let blockTexture = Sym.ofString "blockTexture"

type Vertex = { Position: V3f; Normal: V3f; Uv: V2f }

type Mesh =
    { Vertices: Vertex array
      Triangles: int array }

let empty = { Vertices = [||]; Triangles = [||] }

let create vertices triangles =
    { Vertices = vertices
      Triangles = triangles }

let merge mesh1 mesh2 =
    { Vertices = Array.append mesh1.Vertices mesh2.Vertices
      Triangles = Array.append mesh1.Triangles (mesh2.Triangles |> Array.map ((+) mesh1.Vertices.Length)) }

let createGeometry mesh =
    IndexedGeometry(
        Mode = IndexedGeometryMode.TriangleList,
        IndexArray = mesh.Triangles,
        SingleAttributes =
            SymDict.ofList
                // [ DefaultSemantic.DiffuseColorTexture, blockTexSymbol :> obj
                [ DefaultSemantic.ColorTexture, blockTexture :> obj ],
        IndexedAttributes =
            SymDict.ofList
                [ DefaultSemantic.Positions, mesh.Vertices |> Array.map (fun p -> p.Position) :> Array
                  DefaultSemantic.Normals, mesh.Vertices |> Array.map (fun p -> p.Normal) :> Array
                  DefaultSemantic.DiffuseColorCoordinates, mesh.Vertices |> Array.map (fun p -> p.Uv) :> Array ]
    )
