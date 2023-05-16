module Agents

open System.Threading.Tasks

let simpleAgent task =
    MailboxProcessor<_>.Start(fun inbox ->
        let rec messageLoop () =
            async {
                let! msg = inbox.Receive()

                task msg

                return! messageLoop ()
            }

        messageLoop ())

let taskPoolAgent task =
    let startEvent = new Event<unit>()

    startEvent,
    MailboxProcessor<_>.Start(fun inbox ->
        startEvent.Publish |> Async.AwaitEvent |> Async.RunSynchronously

        let rec messageLoop tasks =
            async {
                let! msg = inbox.TryReceive 100

                let tasks =
                    tasks
                    |> List.append (
                        match msg with
                        | Some msg -> [ Task.Run(action = fun () -> task msg) ]
                        | None -> []
                    )

                // Partition into complete and still running
                let complete, tasks = tasks |> List.partition (fun t -> t.IsCompleted)

                // Delete all the completed tasks
                complete |> List.iter (fun t -> t.Dispose())

                return! messageLoop tasks
            }

        messageLoop [])
