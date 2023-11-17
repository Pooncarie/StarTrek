module Menu

open State
open Input

type Menu = 
    {
      Command : string
      Text : string
      Function: State -> State
      Exit: bool
    }

let inputValidMenuOption (prompt : string) commands =
    let mutable str = ""
    let mutable isOk = false

    while not isOk do
        printf $"{prompt}"
        match getStringInRange readLine (commands |> List.map(fun x -> x.Command)) with
        | Some x -> str <- x; isOk <- true
        | None ->
            printfn "   "
            printfn "ENTER ONE OF THE FOLLOWING:"
            commands |> List.iter(fun mnu -> printfn $"{mnu.Command} - {mnu.Text}")
            printfn "   "
            isOk <- false

    commands |> List.find(fun x -> x.Command = str)

