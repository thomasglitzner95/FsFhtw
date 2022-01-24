[<EntryPoint>]
let main argv =
    printfn "Welcome to the Train configurator!"
    printfn "Please enter your commands to interact with the system."
    printfn "Help, NewTrain, xxx "
    printfn "Press CTRL+C to stop the program."
    printf "> "

    let initialState = Domain.init () //evtl noch ändern?
    Repl.loop initialState
    0 // return an integer exit code
