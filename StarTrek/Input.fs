﻿module Input

open System

let readLine() = Console.ReadLine().Trim().ToUpper();

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

let inputString (prompt : string) =
    printf $"{prompt}"
    readLine()

let inputCoordinate (prompt : string) =
    printf $"{prompt}"
    let str = readLine()
    let mutable numX = 0
    let mutable numY = 0
    let bits = str.Split(',')

    if bits.Length <> 2 then
        (0, 0)
    else
        let x = bits.[0].Trim()
        let y = bits.[1].Trim()
        if Int32.TryParse(x, &numX) then
            if Int32.TryParse(y, &numY) then
                (numX, numY)
            else
                (0, 0)
        else
            (0, 0)
