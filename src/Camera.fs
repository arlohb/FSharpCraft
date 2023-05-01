module Camera

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Application

let mouseControl (mouse: IMouse) =
    let down = mouse.IsDown MouseButtons.Left
    let location = mouse.Position |> AVal.map (fun pp -> pp.Position)

    adaptive {
        let! d = down

        if d then
            return
                location
                |> AVal.step (fun op delta (cam: CameraView) ->
                    let trafo =
                        M44d.Rotation(cam.Right, float delta.Y * -0.005)
                        * M44d.Rotation(cam.Sky, float delta.X * -0.005)

                    let newForward = trafo.TransformDir cam.Forward |> Vec.normalize
                    cam.WithForward newForward)
        else
            return AdaptiveFunc.Identity
    }

let camera (keyboard: IKeyboard) (mouse: IMouse) (time: aval<DateTime>) (initial: CameraView) : aval<CameraView> =
    AVal.integrate
        initial
        time
        [ (DefaultCameraController.controlWSAD keyboard time)
          (DefaultCameraController.controlLookAround mouse)
          (DefaultCameraController.controlPan mouse)
          (DefaultCameraController.controlZoom mouse)
          (DefaultCameraController.controllScroll mouse time) ]
