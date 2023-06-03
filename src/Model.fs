module Model

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.UI.Primitives
open Adaptify

[<ModelType>]
type Model =
    { camera: CameraControllerState
      boxes: IndexList<Box3d> }
