module Input

open System
open Domain

let private readLine() = Console.ReadLine().Trim().ToUpper();

let inputInteger (prompt : string) =
    printf $"{prompt}"
    let str = readLine()
    let mutable num = 0

    if Int32.TryParse(str, &num) then
        num
    else
        0

let inputDouble (prompt : string) =
    printf $"{prompt}"
    let str = readLine()
    let mutable num = 0.0

    if Double.TryParse(str, &num) then
        num
    else
        0

let inputDoubleInRange prompt range =
    let mutable num = 0.0
    let mutable isOk = false

    while not isOk do
        printf $"{prompt}"
        let str = readLine()
        if Double.TryParse(str, &num) then
            if range |> List.contains num then
                isOk <- true

    num

let inputString (prompt : string) =
    printf $"{prompt}"
    readLine()

let inputValidString (prompt : string) commands =
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

    str

let inputCoordinate (prompt : string) =
    printf $"{prompt}"
    let str = readLine()
    let mutable numX = 0.0
    let mutable numY = 0.0
    let bits = str.Split(',')

    if bits.Length <> 2 then
        (0.0, 0.0)
    else
        let x = bits.[0].Trim()
        let y = bits.[1].Trim()
        if Double.TryParse(x, &numX) then
            if Double.TryParse(y, &numY) then
                (numX, numY)
            else
                (0.0, 0.0)
        else
            (0.0, 0.0)
