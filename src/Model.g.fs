//89d43383-2380-db01-4859-e34b66f10c6e
//f66d75df-92d5-3be6-bbf0-2d4455a157db
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "1182" // value is unused
namespace rec global

open System
open FSharp.Data.Adaptive
open Adaptify
[<AutoOpen>]
module rec Adaptify =
    module rec Model =
        [<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
        type AdaptiveModel(value : Model.Model) =
            let _camera_ = Aardvark.UI.Primitives.AdaptiveCameraControllerState(value.camera)
            let _boxes_ = FSharp.Data.Adaptive.clist(value.boxes)
            let mutable __value = value
            let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
            static member Create(value : Model.Model) = AdaptiveModel(value)
            static member Unpersist = Adaptify.Unpersist.create (fun (value : Model.Model) -> AdaptiveModel(value)) (fun (adaptive : AdaptiveModel) (value : Model.Model) -> adaptive.Update(value))
            member __.Update(value : Model.Model) =
                if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Model.Model>.ShallowEquals(value, __value))) then
                    __value <- value
                    __adaptive.MarkOutdated()
                    _camera_.Update(value.camera)
                    _boxes_.Value <- value.boxes
            member __.Current = __adaptive
            member __.camera = _camera_
            member __.boxes = _boxes_ :> FSharp.Data.Adaptive.alist<Aardvark.Base.Box3d>

