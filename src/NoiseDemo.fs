module NoiseDemo

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering

let size = 256

let image = new PixImage<byte>(Col.Format.RGBA, V2i.II * size)

let draw (time: System.DateTime) =
    let offset = float time.Millisecond / 1000.0

    image.GetMatrix<C4b>().SetByCoord(fun _ -> C4b(offset, offset, 1.0 - offset))
    |> ignore

draw System.DateTime.Now

let texture time =
    let mutable lastTime = System.DateTime.Now

    time
    |> AVal.map (fun now ->
        if (now - lastTime).TotalMilliseconds >= 10 then
            lastTime <- now
            draw now

        new PixTexture2d(image) :> ITexture)
