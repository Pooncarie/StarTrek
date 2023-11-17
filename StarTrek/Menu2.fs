module Menu2

open Input
open State

type Menu2 = {
    Key : string
    Text : string
    Function: State -> string list
    Exit : bool
    }

let inputValidMenuOption2 (prompt : string) commands =
    let mutable str = ""
    let mutable isOk = false

    while not isOk do
        printf $"{prompt}"
        match getStringInRange readLine (commands |> List.map(fun x -> x.Key)) with
        | Some x -> str <- x; isOk <- true
        | None ->
            printfn "   "
            printfn "ENTER ONE OF THE FOLLOWING:"
            commands |> List.iter(fun mnu -> printfn $"{mnu.Key} - {mnu.Text}")
            printfn "   "
            isOk <- false

    commands |> List.find(fun x -> x.Key = str)
