module Camera

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Application

let camera (keyboard: IKeyboard) (mouse: IMouse) (time: aval<DateTime>) (initial: CameraView) : aval<CameraView> =
    AVal.integrate
        initial
        time
        [ (DefaultCameraController.controlWSADwithSpeed (cval 200) keyboard time)
          (DefaultCameraController.controlLookAround mouse)
          (DefaultCameraController.controlPan mouse)
          (DefaultCameraController.controlZoom mouse)
          (DefaultCameraController.controllScroll mouse time) ]

let chunksAroundCamera (location: V3d) xyRadius zMin zMax =
    let chunk = V3i(location / float Chunk.Size)

    Lib.aroundPointXY xyRadius zMin zMax chunk |> Array.map (V3i >> Chunk.Id)
