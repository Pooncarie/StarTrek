module Computer

open System
open Domain
open QuadrantNames
open Input

let private distanceCalculator coords = 
    let x = coords.FinalY - coords.InitialY 
    let y = coords.FinalX - coords.InitialX 

    let d = match (x, y) with
                | (x,y) when y = 0 && x > 0 -> 1
                | (x,y) when y = 0 && x < 0 -> 5
                | (x,y) when x = 0 && y > 0 -> 7
                | (x,y) when x = 0 && y < 0 -> 3
                | (x,y) when x > 0 && y > 0 -> 8
                | (x,y) when x > 0 && y < 0 -> 2
                | (x,y) when x < 0 && y < 0 -> 4
                | (x,y) when x < 0 && y > 0 -> 6
                | _ ->  0
       
    printfn $"DIRECTION = {d}"
    printfn $"DISTANCE = {(sqrt (double (square y + square x))):N2 }"
            
let private computerStatusReport state =
    printfn "STATUS REPORT:"
    printfn $"    KLINGONS LEFT: {state.TotalKlingons}"
    printfn $"    MISSION MUST BE COMPLETED IN {state.NumberOfStarDays} STARDATES"
    if state.TotalStarbases > 0 then
        printfn $"    THE FEDERATION IS MAINTAINING {state.TotalStarbases} STARBASES IN THE GALAXY"
    else
        printfn "    YOUR STUPIDITY HAS LEFT YOU ON YOUR ON IN THE GALAXY -- YOU HAVE NO STARBASES LEFT!"

let private computerPhotonTorpedoData(state : State) =
    getKlingons state |> List.iter(fun klingon -> 
        printf "FROM ENTERPRISE TO KLINGON BATTLE CRUISER "; 
        distanceCalculator { 
            InitialX = double (fst state.CurrentSector + 1);
            InitialY = double (snd state.CurrentSector + 1);
            FinalX = double (fst klingon.SectorId + 1);
            FinalY = double (snd klingon.SectorId + 1)})

let private computerStarbaseData state =
    let starbases = getStarbases state

    if starbases.Length = 0 then
        printfn "MR. SPOCK REPORTS, 'SENSORS SHOW NO STARBASES IN THIS QUADRANT.'"
    else
        starbases |> List.iter(fun starbase -> 
            printf "FROM ENTERPRISE TO STARBASE " 
            distanceCalculator { 
                InitialX = double (fst state.CurrentSector + 1);
                InitialY = double (snd state.CurrentSector + 1); 
                FinalX = double (fst starbase.SectorId + 1); 
                FinalY = double (snd starbase.SectorId + 1)})

(* LINE 8150 *)   
let private directionDistanceCalculator state =
    printfn "DIRECTION/DISTANCE CALCULATOR:"
    printfn $"YOU ARE AT QUADRANT {fst state.CurrentQuadrant + 1},{snd state.CurrentQuadrant + 1} SECTOR {fst state.CurrentSector + 1},{snd state.CurrentSector + 1}"
    printfn ""
    let initCoordinate = inputCoordinate "PLEASE ENTER INITIAL COORDINATES (X,Y): "
    let finalCoordinate = inputCoordinate "PLEASE ENTER FINAL COORDINATES (X,Y): "

    let coords = {
        InitialX = fst initCoordinate
        InitialY = snd initCoordinate
        FinalX = fst finalCoordinate
        FinalY = snd finalCoordinate
        }

    distanceCalculator coords

let private galaxyMap state  = 
    printfn $"CURRENT QUADRANT {(quadrantName state.CurrentQuadrant)}"
    printfn "+-I-----------+-II----------+-III---------+-IV----------+-I-----------+-II----------+-III---------+-IV----------+"
    for i in quadrantRange do
        printf "| "
        for j in quadrantRange do
            let name = quadrantNameAlt (i, j)
            printf $"{name} | "
        printfn ""
        printfn "+-------------+-------------+-------------+-------------+-------------+-------------+-------------+-------------+"

let private validNav =
    printfn "NAVIGATION DIRECTIONS"
    printfn "====================="
    printfn "4   3   2"
    printfn "  \ ' /"
    printfn "5 - * - 1"
    printfn "  / ' \ "
    printfn "6   7   8"

let computer state =
    let commandMenu() =
        printfn "   "
        printfn "FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER::"
        printfn "0 = CUMULATIVE GALACTIC RECORD"
        printfn "1 = STATUS REPORT"
        printfn "2 = PHOTON TORPEDO DATA"
        printfn "3 = STARBASE NAV DATA"
        printfn "4 = DIRECTION/DISTANCE CALCULATOR"
        printfn "5 = GALAXY MAP"
        printfn "6 = NAVIGATION DIRECTIONS"
        printfn "7 = EXIT LIBRARY-COMPUTER"
        printfn "   "
        inputString "COMMAND ? "

    let getCommand() =
        let mutable validCommand = false;
        let mutable cmdOption = ""

        while not validCommand do
            let mutable cmd = commandMenu()
            cmdOption <- match cmd with
                            | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" -> validCommand <- true; cmd;
                            | _ -> printfn "Invalid command"; "";
            printfn ""

        cmdOption

    let mutable isOk = true
    let mutable st = state

    while isOk do
        match getCommand() with
                | "1" -> computerStatusReport state
                | "2" -> computerPhotonTorpedoData state
                | "3" -> computerStarbaseData state
                | "4" -> directionDistanceCalculator state
                | "5" -> galaxyMap state
                | "6" -> validNav 
                | "7" -> isOk <- false; 
                | _ -> ();

    state