module Domain

type Waggons =
    {
    name: string
    weightInKg : int
    quantity : int }


type Lokomotive =
    {
    lokName : string
    powerInKg : int
    }

type LokStatus =
    | NoLok
    | SomeLok of Lokomotive

type Train =
    {
    lokStatus : LokStatus
    waggons : list<Waggons> }

let init () : Train =
    {
    lokStatus = NoLok
    waggons = [] }

type FailureReason =
    | NoWaggonsFailure
    | NoLokFoundFailure

type AnkopplungsResult =
    | LokAlreadyActiveFailure
    | AnkopplungSuceess
    | AnkopplungFailed of FailureReason

type NewTrain = Train -> Train
//type TrainStatusOverview = Train -> Train
type AddWaggon = Train -> Waggons -> Train
type RemoveWaggon = Train -> Waggons -> Train
type AddLok = Train -> Lokomotive -> Train
type RemoveLok = Train -> Lokomotive -> Train

type Message =
    | NewTrain of Train //starts from a fresh state
//| TrainStatusOverview of Train //of current train and its waggons
    | AddWaggon of Waggons
    | RemoveWaggon of Waggons
    | AddLok of Lokomotive
    | RemoveLok of string
//    | Dock

let NewTrain (train : Train)=
    { lokStatus = NoLok ; waggons = [] }

type TrainStatus =
    | EmptyTrain
    | TrainWithoutWaggons
    | TrainWithoutLok
    | FinishedTrain
//    | DockedTrain

//let TrainStatusOverview (train: Train) =
//    match train.waggons with
//    | [] ->
//        match train.lokomotive with
//            | [] ->
//                Trainstatus = EmptyTrain
//            | Some Lokomotive ->
//                    Trainstatus = TrainWithoutWaggons
//    | Some waggons ->
//        match train.lokomotive with
//            | [] ->
//                Trainstatus = TrainWithoutLok
//            | Some waggons ->
//                    Trainstatus = FinishedTrain

let AddWaggon (train : Train) (newWaggons) = //muss das komplette waggonModell übergeben
    { lokStatus = train.lokStatus ; waggons = newWaggons :: train.waggons }

let RemoveWaggon (train : Train) (waggonName : string) =
    { lokStatus = train.lokStatus ; waggons = train.waggons|> List.filter(fun w -> w.name = waggonName)}

//match train.waggons with
//| List<waggons> -> 
//| _ -> FailureReason(KeineWaggons("No waggons to remove"))

let AddLok (train : Train) (lok : Lokomotive)  = //(lokname : string) (power : decimal)
    match train.lokStatus with
    | NoLok -> SomeLok { lokName = lok.lokName ; powerInKg = lok.powerInKg }
    | SomeLok lok -> SomeLok lok //|LokAlreadyActiveFailure("This train already has a lok")

let RemoveLok (train : Train) =
    match train.lokStatus with
    | SomeLok lok ->  NoLok
    | NoLok -> NoLok //|NoLokFoundFailure("This train has no lokomotive to remove")

let update (msg : Message) (oldTrain : Train) : Train =
    match msg with
    | NewTrain msg -> NewTrain msg
    | AddWaggon waggons -> AddWaggon oldTrain waggons
    | RemoveWaggon waggonName -> RemoveWaggon oldTrain waggonName
    | AddLok lok -> AddLok oldTrain lok
    | RemoveLok msg -> RemoveLok oldTrain
//    | TrainStatusOverview -> Domain.Overview oldTrain
//    | Dock -> Domain.Dock oldTrain
