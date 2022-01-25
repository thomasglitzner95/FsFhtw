module Parser

open System
open Domain

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|NewTrain|AddWaggon|RemoveWaggon|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg

        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray

    match parts with
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb ] when safeEquals verb (nameof Domain.NewTrain) -> NewTrain
//    | [ verb ] when safeEquals verb (nameof Domain.Overview) -> Overview
    | [ verb; name; weight; amount ] when safeEquals verb (nameof Domain.AddWaggon) ->
        tryParseInt weight (fun weight -> tryParseInt amount (fun amount -> AddWaggon { name = name ; weightInKg = weight ; quantity = amount}))
    | [ verb ; name ] when safeEquals verb (nameof Domain.RemoveWaggon) -> RemoveWaggon name
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed


let (|AddLok|RemoveLok|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb; name; force ] when safeEquals verb (nameof Domain.AddLok) -> tryParseInt force (fun force -> AddLok { lokName = name ; powerInKg = force })
    | [ verb; name ] when safeEquals verb (nameof Domain.RemoveLok) -> RemoveLok name
//| [ verb, arg ] when verb = "reset" -> Reset //so zusätzliche cases einbauen
    | _ -> ParseFailed
