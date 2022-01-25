module OLD

type WaggonName = string
type WaggonWeight = decimal
type WaggonQuantity = decimal

type Waggons =
    {
    name: WaggonName
    weightInKg : WaggonWeight
    quantity : WaggonQuantity
    }

type LokomotiveType =
    | ElectricLok
    | SteamLok

type CarrierPower = decimal

type Lokomotive =
    {
    lokType : LokomotiveType
    powerInKg : CarrierPower
    }

type EmptyTrain = EmptyTrain

type TrainWithoutWaggons =
    {
    lokomotive: Lokomotive }

type TrainWithoutLok =
    {
    waggons : list<Waggons> }

type DockingStatus =
    | Undocked
    | Docked

type FinishedTrainState =
    {
    dockingStatus : DockingStatus
    lokomotive: Lokomotive
    waggons : list<Waggons> }

type TrainwithoutLokState =
    {
    waggons : list<Waggons> }

    //list<Waggons>
type TrainWithoutWaggonsState = Lokomotive

type Train =
    | EmptyTrain
    | TrainWithoutWaggons of TrainWithoutWaggonsState
    | TrainWithoutLok of TrainwithoutLokState
    | FinishedTrain of FinishedTrainState
//| DockedTrain

let init () : Train = EmptyTrain

type FailureReason =
    | KeineWaggons
    | KeineLok

type AnkopplungsResult =
    | LokAlreadyActiveFailure
    | AnkopplungSuceess
    | AnkopplungFailed of FailureReason

type NewTrain = Train -> Train
type Overview = Train -> Train
type AddWaggon = Train -> Waggons -> Train
type RemoveWaggon = Train -> Waggons -> Train
type AddLok = Train -> Lokomotive -> Train
type RemoveLok = Train -> Lokomotive -> Train

type Message =
    | NewTrain of Train //starts from a fresh state
    | Overview of Train //of current train and its waggons
    | AddWaggon of Waggons
    | RemoveWaggon of Waggons
    | AddLok of Lokomotive
    | RemoveLok of Lokomotive
    | Dock

//let NewTrain (train : Train) =
//    match train with
//    |_ -> EmptyTrain
//    | FinishedTrain ->
//        match DockingStatus with
//            | Docked -> Some //Error
//            | Undocked
//                FinishedTrain -> NewTrain

//let Overview (train: Train) (lokomotive : Lokomotive)=
//    match train with
//    | EmptyTrain -> EmptyTrain
//    | TrainWithoutWaggons -> TrainWithoutWaggons lokomotive = Lokomotive
//    | TrainWithoutLok -> TrainWithoutLok { waggons = waggons = [{ name = name; weightInKg = weightInKg; quantity = quantity } ]}
//    | FinishedTrain -> FinishedTrain { dockingStatus = DockingStatus ; lokomotive = Lokomotive ; waggons = waggons = [{ name = name; weightInKg = weightInKg; quantity = quantity } ]}

let AddWaggon (train : Train) (name : WaggonName) (weightInKg : WaggonWeight) (amount : WaggonQuantity) =
    match train with
    | EmptyTrain -> TrainWithoutLok { waggons = [{ name = name; weightInKg = weightInKg; quantity = amount }] }
    | TrainWithoutLok train -> TrainWithoutLok { waggons = { name = name; weightInKg = weightInKg; quantity = amount } :: train.waggons }
    | TrainWithoutWaggons train -> FinishedTrain { dockingStatus = Undocked ; lokomotive = TrainWithoutWaggons.lokomotive. ; waggons = { name = name; weightInKg = weightInKg; quantity = amount } :: train.waggons }
    | FinishedTrain -> FinishedTrain (waggons = { name = name; weightInKg = weightInKg; quantity = quantity} :: waggons )

let RemoveWaggon (Train : Train) (name : WaggonName) (weightInKg : WaggonWeight) (quantity : WaggonQuantity) =
    match Train with
    | EmptyTrain|TrainWithoutWaggons -> None //FailureReason (KeineWaggons("Fehlermeldung")) <|ignore
    | TrainWithoutLok -> None
    | FinishedTrain -> None
    | _ -> None
    if Train.waggons = [] then Train(FinishedTrain > TrainWithoutWaggons) else FinishedTrain>FinishedTrain (waggons = { name = name; weightInKg = weightInKg; quantity = quantity} :: waggons )


let AddLok (train : Train) (lokomotive) =
    match train with
    | EmptyTrain -> TrainWithoutWaggons {lokomotive = lokomotive}
    | TrainWithoutLok -> FinishedTrain {lokomotive = lokomotive}
    | TrainWithoutWaggons -> LokAlreadyActiveFailure("This train already has a lok")
    | FinishedTrain -> LokAlreadyActiveFailure("This train already has a lok")

let RemoveLok (train : Train) (lokType : LokomotiveType) (powerInKg : CarrierPower ) =
    match train with
    | TrainWithoutLok -> NoLok
    | TrainWithoutWaggons -> NoLok
    | EmptyTrain -> NoLok


type Dock = Train -> ShoppingCart -> PaymentMethod -> PaymentResult

//weitere überprüfungsfunktion
let Dock (Train : Train) (Lokomotive : Lokomotive) : Train =
    match Train with
    | FinishedTrain ->
        match FinishedTrain.dockingstatus with
            |Docked -> Docked //Error
            |Undocked -> Undocked
                //if FinishedTrain. > FinishedTrain.lokomotive.powerInKg then 
                
    | _ ->  None //error, only Finished-Trains without undocked status can be checked out


let dock : Dock =
    fun dock Lokomotive waggons ->
        match Train with
        | UnfertigerZug _
        | ZugOhneWaggons _ ->
            WaggonOfAmount
            |_ -> Some

let trainApi: TrainApi =
    {
    NewTrain = NewTrain
    Overveiew = Overview
    AddWaggon = AddWaggon
    RemoveWaggon = RemoveWaggon
    AddLok = AddLok
    RemoveLok = RemoveLok
    }

let update (msg : Message) (oldTrain : Train) : Train =
    match msg with
    | NewTrain -> trainApi.NewTrain oldTrain.
    | Overview -> trainApi.Overview oldTrain
    | AddWaggon waggons -> trainApi.AddWaggon oldTrain waggons
    | RemoveWaggon waggonName -> trainApi.RemoveWaggon oldTrain waggonName
    | AddLok lokName -> trainApi.AddLok oldTrain lokName
    | RemoveLok -> trainApi.RemoveLok oldTrain
