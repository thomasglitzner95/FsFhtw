module Domain

type Waggons =
    {
    name: string
    weightInKg : decimal
    quantity : decimal }

type Lokomotive =
    {
    lokName : string
    powerInKg : decimal
    }

type Train =
    {
    trainStatus : TrainStatus
    lokomotive : Lokomotive
    waggons : list<Waggons> }

let init () : Train =
    {
    lokomotive = []
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
    | TrainStatusOverview of Train //of current train and its waggons
    | AddWaggon of Waggons
    | RemoveWaggon of Waggons
    | AddLok of Lokomotive
    | RemoveLok of Lokomotive
//    | Dock

let NewTrain (train : Train)=
    match train.trainStatus with
//    | DockedTrain -> DockedTrain
    | _ -> { trainStatus = EmptyTrain; lokomotive = [] ; waggons = [] }

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
    { lokomotive = train.lokomotive ; waggons = newWaggons :: train.waggons }

let RemoveWaggon (train : Train) (waggonName : WaggonName) =
    match train.waggons with
    | Some -> { lokomotive = train.lokomotive ; waggons = train.waggons.filter(fun w -> w.name.equals(waggonName)) }
    | _ -> FailureReason(KeineWaggons("No waggons to remove"))

let AddLok (train : Train) (lok : Lokomotive)  = //(lokname : string) (power : decimal)
    match train.lokomotive with
    | [] -> { lokomotive = lok ; waggons = train.waggons }
    | Some -> LokAlreadyActiveFailure("This train already has a lok")

let RemoveLok (train : Train) =
    match train.lokomotive with
    | Some lokomotive -> { lokomotive = [] ; waggons = train.waggons }
    | [] -> NoLokFoundFailure("This train has no lokomotive to remove")


let update (msg : Message) (oldTrain : Train) : Train =
    match msg with
    | NewTrain name -> Domain.NewTrain msg.name
//    | TrainStatusOverview -> Domain.Overview oldTrain
    | AddWaggon waggons -> Domain.AddWaggon oldTrain waggons
    | RemoveWaggon waggonName -> Domain.RemoveWaggon oldTrain waggonName
    | AddLok lokName -> Domain.AddLok oldTrain lokName
    | RemoveLok -> Domain.RemoveLok oldTrain
//    | Dock -> Domain.Dock oldTrain


