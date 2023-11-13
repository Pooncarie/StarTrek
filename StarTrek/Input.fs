module Input

open System
open Domain

let private readLine() : string = Console.ReadLine().Trim().ToUpper();

let private  getInteger (lineReader: unit -> string)  : int option =
    let str = lineReader()
    let mutable num = 0

    if Int32.TryParse(str, &num) then
        Some num
    else
       None

let private  getDouble (lineReader: unit -> string)  : double option =
    let str = lineReader()
    let mutable num = 0.0

    if Double.TryParse(str, &num) then
        Some num
    else
        None

let private getDoubleInRange (lineReader: unit -> string)  range : double option =
    let str = lineReader()
    let mutable num = 0.0

    if Double.TryParse(str, &num) then
        if range |> List.contains num then
            Some num
        else
            None
    else
        None

let inputInteger (prompt : string) =
    let mutable isOk = false
    let mutable num = 0
    while not isOk do
        printf $"{prompt}"
        getInteger readLine |> function
            | Some x -> num <- x; isOk <- true
            | None -> printfn "!NUMBER EXPECTED - RETRY INPUT LINE"; isOk <- false
    num

let inputDouble (prompt : string) =
    let mutable isOk = false
    let mutable num = 0.0
    while not isOk do
        printf $"{prompt}"
        getDouble readLine |> function
            | Some x -> num <- x; isOk <- true
            | None -> printfn "!NUMBER EXPECTED - RETRY INPUT LINE"; isOk <- false
    num

let inputDoubleInRange (prompt : string) range =
    let mutable num = 0.0
    let mutable isOk = false

    while not isOk do
        printf $"{prompt}"
        getDoubleInRange readLine range |> function
            | Some x -> num <- x; isOk <- true
            | None -> printfn $"!NUMBER IN {range |> List.min} - {range |> List.max} EXPECTED - RETRY INPUT LINE"; isOk <- false

    num

let inputString (prompt : string) =
    printf $"{prompt}"
    readLine()

let inputValidMenuOption (prompt : string) commands =
    let mutable str = ""
    let mutable isOk = false

    while not isOk do
        printf $"{prompt}"
        str <- readLine()
        if commands |> List.exists(fun x -> x.Command = str) then
            isOk <- true
        else
            printfn "   "
            printfn "ENTER ONE OF THE FOLLOWING:"
            commands |> List.iter(fun mnu -> printfn $"{mnu.Command} - {mnu.Text}")
            printfn "   "
            isOk <- false

    commands |> List.find(fun x -> x.Command = str)

let inputValidMenuOption2 (prompt : string) commands =
    let mutable str = ""
    let mutable isOk = false

    while not isOk do
        printf $"{prompt}"
        str <- readLine()
        if commands |> List.exists(fun x -> x.Key = str) then
            isOk <- true
        else
            printfn "   "
            printfn "ENTER ONE OF THE FOLLOWING:"
            commands |> List.iter(fun mnu -> printfn $"{mnu.Key} - {mnu.Text}")
            printfn "   "
            isOk <- false

    commands |> List.find(fun x -> x.Key = str)

let inputCoordinate (prompt : string) =
    printf $"{prompt}"
    let str = readLine()
    let mutable numX = 0.0
    let mutable numY = 0.0
    let bits = str.Split(',')

    if bits.Length <> 2 then
        None
    else
        let x = bits.[0].Trim()
        let y = bits.[1].Trim()
        if Double.TryParse(x, &numX) then
            if Double.TryParse(y, &numY) then
                if sectorRange |> List.contains(int numX-1) && sectorRange |> List.contains(int  numY-1) then
                    Some (numX, numY)
                else
                    None
            else
                None
        else
            None
