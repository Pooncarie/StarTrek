module Computer

open System
open Domain
open QuadrantNames
open Input

let private distanceCalculator (state : State) coords = 
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
    ()

let private computerStatusReport state =
    printfn "STATUS REPORT:"
    printfn $"    KLINGONS LEFT: {state.TotalKlingons}"
    printfn $"    MISSION MUST BE COMPLETED IN {state.NumberOfStarDays - (state.StarDate - state.StartedOnStardate)} DAYS"
    if state.TotalStarbases > 0 then
        printfn $"    THE FEDERATION IS MAINTAINING {state.TotalStarbases} STARBASES IN THE GALAXY"
    else
        printfn "    YOUR STUPIDITY HAS LEFT YOU ON YOUR ON IN THE GALAXY -- YOU HAVE NO STARBASES LEFT!"
    state

let private computerPhotonTorpedoData state =
    getKlingons state |> List.iter(fun klingon -> 
        printf "FROM ENTERPRISE TO KLINGON BATTLE CRUISER "; 
        distanceCalculator state { 
            InitialX = double (fst state.CurrentSector + 1);
            InitialY = double (snd state.CurrentSector + 1);
            FinalX = double (fst klingon.SectorId + 1);
            FinalY = double (snd klingon.SectorId + 1)}) 
    state

let private computerStarbaseData state =
    let starbases = getStarbases state

    if starbases.Length = 0 then
        printfn "MR. SPOCK REPORTS, 'SENSORS SHOW NO STARBASES IN THIS QUADRANT.'"
    else
        starbases |> List.iter(fun starbase -> 
            printf "FROM ENTERPRISE TO STARBASE " 
            distanceCalculator state { 
                InitialX = double (fst state.CurrentSector + 1);
                InitialY = double (snd state.CurrentSector + 1); 
                FinalX = double (fst starbase.SectorId + 1); 
                FinalY = double (snd starbase.SectorId + 1)})
    state

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

    distanceCalculator state coords
    state


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
    state

let private validNav (state : State) =
    printfn "NAVIGATION DIRECTIONS"
    printfn "====================="
    printfn "4   3   2"
    printfn "  \ ' /"
    printfn "5 - * - 1"
    printfn "  / ' \ "
    printfn "6   7   8"
    state

let computer state =
    let menu =
        [
            { Command = "0"; Text = "CUMULATIVE GALACTIC RECORD"; Function = computerStatusReport; Exit = false }
            { Command = "1"; Text = "STATUS REPORT"; Function = computerStatusReport; Exit = false }
            { Command = "2"; Text = "PHOTON TORPEDO DATA"; Function = computerPhotonTorpedoData; Exit = false }
            { Command = "3"; Text = "STARBASE NAV DATA"; Function = computerStarbaseData; Exit = false }
            { Command = "4"; Text = "DIRECTION/DISTANCE CALCULATOR"; Function = directionDistanceCalculator; Exit = false }
            { Command = "5"; Text = "GALAXY MAP"; Function = galaxyMap; Exit = false }
            { Command = "6"; Text = "NAVIGATION DIRECTIONS"; Function = validNav; Exit = false }
            { Command = "7"; Text = "EXIT LIBRARY-COMPUTER"; Function = (fun state -> state); Exit = true }
        ]

    let mutable state = state
    let mutable isOk = true

    while isOk do
        let cmd = inputValidMenuOption "COMPUTER ACTIVE AND AWAITING COMMAND? " menu
        match cmd.Exit with
        | true -> isOk <- false; state <- cmd.Function state
        | false -> state <- cmd.Function state

    
    state