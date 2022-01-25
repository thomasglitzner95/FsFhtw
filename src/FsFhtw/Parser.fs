module Parser

open System
open Domain

let safeEquals (it : string) (theOther : string) =
    String.Equals(it, theOther, StringComparison.OrdinalIgnoreCase)

[<Literal>]
let HelpLabel = "Help"

let (|NewTrain|Overview|AddWaggon|RemoveWaggon|Help|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray

    match parts with
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | [ verb ] when safeEquals verb (nameof Domain.Train) -> NewTrain
//    | [ verb ] when safeEquals verb (nameof Domain.Train) -> Overview
    | [ verb; name; weight; amount ] when safeEquals verb (nameof Domain.Train) ->
    tryParseInt weight (fun w -> tryParseInt amount (fun a -> AddWaggon { name = name ; weight = weight ; amount = amount}))
    | [ verb ] when safeEquals verb (nameof Domain.Train) -> RemoveWaggon name
    | [ verb ] when safeEquals verb HelpLabel -> Help
    | _ -> ParseFailed


let (|AddLok|RemoveLok|ParseFailed|) (input : string) =
    let tryParseInt (arg : string) valueConstructor =
        let (worked, arg') = Int32.TryParse arg
        if worked then valueConstructor arg' else ParseFailed

    let parts = input.Split(' ') |> List.ofArray
    match parts with
    | [ verb; name; force ] when safeEquals verb (nameof Domain.Train) -> 
    tryParseInt force (fun f -> tryParseInt amount (fun a -> AddLok { name = name ; force = force}))
    | [ verb; name ] when safeEquals verb (nameof Domain.Train) -> RemoveLok name
    | [ verb, arg ] when verb = "reset" -> Reset //so zusätzliche cases einbauen
    | _ -> ParseFailed
