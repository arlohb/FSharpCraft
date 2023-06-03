module App

open FSharp.Collections
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.UI
open Aardvark.UI.Primitives

open Model

type Action =
    | CameraMessage of FreeFlyController.Message
    | NewBox of Box3d

let update (m: Model) (a: Action) =
    match a with
    | CameraMessage msg ->
        { m with
            camera = FreeFlyController.update m.camera msg }
    | NewBox box -> { m with boxes = m.boxes.Add box }

let createBox () =
    let random = new System.Random()
    let x = random.NextDouble() * 2.0 - 1.0
    let y = random.NextDouble() * 2.0 - 1.0
    let z = random.NextDouble() * 2.0 - 1.0

    Box3d.FromCenterAndSize(5.0 * V3d(x, y, z), V3d(1))

let view (m: AdaptiveModel) =
    let frustrum = AVal.constant (Frustum.perspective 60.0 0.1 100.0 1.0)

    require
        Html.semui
        (body [] [
            div [] [
                (FreeFlyController.controlledControl
                    m.camera
                    (fun m -> CameraMessage m)
                    frustrum
                    (AttributeMap.ofList [ attribute "style" "width:65%; height: 100%; float: left;" ])
                    (
                        m.boxes
                        |> AList.fold (fun boxes box ->
                        (
                            Sg.box (AVal.constant C4b.Green) (AVal.constant box)
                        )::boxes ) []
                        |> AVal.map (fun sg ->
                            sg
                            |> Sg.ofList
                            |> Sg.shader {
                                do! DefaultSurfaces.trafo
                                do! DefaultSurfaces.vertexColor
                                do! DefaultSurfaces.simpleLighting
                            }
                            |> Sg.noEvents
                        )
                        |> Sg.dynamic
                    )
                )

                button [ clazz "ui button"; onClick (createBox >> NewBox)] [ text "+" ]

                Incremental.text ( m.boxes.Content |> AVal.map (fun boxes -> sprintf "%d" boxes.Count ))
            ]
        ])

let app =
    { unpersist = Unpersist.instance
      threads = fun _ -> ThreadPool.empty
      initial =
        { Model.camera = FreeFlyController.initial
          boxes = IndexList.empty }
      update = update
      view = view }
