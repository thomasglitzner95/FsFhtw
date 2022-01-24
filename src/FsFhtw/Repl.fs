module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Message
    | HelpRequested
    | NotParsable of string

type Train = Domain.Train //state

let read (input : string) =
    match input with
    | NewTrain -> Domain.NewTrain |> DomainMessage
    | Overview -> Domain.Overview |> DomainMessage
    | AddWaggon waggons -> Domain.AddWaggon waggons |> DomainMessage
    | RemoveWaggon waggonName -> Domain.RemoveWaggon waggonName |> DomainMessage
    | AddLokomotive lok -> Domain.AddLokomotive lok |> DomainMessage
    | RemoveLokomotive -> Domain.RemoveLokomotive |> DomainMessage
    | Help -> HelpRequested
    | Reset -> Domain.Reset |> DomainMessage
    | ParseFailed  -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Message>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

//let evaluateTrain (msg: Domain.Message) (Train: Train) (result: Domain.Result)
//    match result with
//    | Domain.Train train ->
//        match msg with
//        |Domain.

let evaluate (update : Domain.Message -> Train -> Train) (train : Train) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg train
        let message = sprintf "The message was %A. New state is %A" msg newState
        (newState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (train, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (train, message)

let print (train : Domain.Train, outputToPrint : string) = //Domain.Train or just Train ?
    printfn "%s\n" outputToPrint
    printf "> "
    train

let rec loop (train : Train) =
    Console.ReadLine()
    |> read
    |> evaluate Domain.update train
    |> print
    |> loop
