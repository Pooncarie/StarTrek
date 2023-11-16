module Computer

open Domain
open Klingon
open Input
open Starbase
open QuadrantNames
open State
open Menu

(* copied from c# version at coding-horror/basic-computer-games *)
let private getDirection (deltaX : double) (deltaY : double) =
    let deltaXDominant = abs deltaX > abs deltaY
    let fractionalPart = if deltaXDominant then deltaY / deltaX else -deltaX / deltaY
    let nearestCardinal = match deltaXDominant with
                            | true -> if deltaX > 0 then 7.0 else 3.0
                            | false -> if deltaY > 0 then 1.0 else 5.0
    let direction = nearestCardinal + fractionalPart
    if direction < 1 then direction + 8.0 else direction

let getDistance deltaX deltaY =
    sqrt (double (square deltaX + square deltaY))

let private remove00 (str : string) = 
    let bits = str.Split('.')
    if bits.Length = 2 then
        if bits[1] = "00" then bits[0] else str
    else
        str

let private doDirectionCalculations coords = 
    let deltaX = coords.FinalX - coords.InitialX
    let deltaY = coords.FinalY - coords.InitialY
    let dirStr = remove00 $"{(getDirection deltaX deltaY):N2}" 
    $"DIRECTION = {dirStr}"

let private doDistanceCalculations coords = 
    let deltaX = coords.FinalX - coords.InitialX
    let deltaY = coords.FinalY - coords.InitialY
    let disStr = remove00 $"{(getDistance deltaX deltaY):N2}"
    $"DISTANCE = {disStr}"

let private getDirectionDistance initCoordinate finalCoordinate  =
    let makeCoordinates initCoordinate finalCoordinate = 
        { 
            InitialX = fst initCoordinate
            InitialY = snd initCoordinate
            FinalX = fst finalCoordinate
            FinalY = snd finalCoordinate
        }

    match (initCoordinate, finalCoordinate) with
    | (Some i, Some j) ->
        let coords = makeCoordinates i j
        [""; doDirectionCalculations coords; doDistanceCalculations coords]
    | (_, _) ->  ["INCORRECT COORDINATES"]

(* Menu option 1 STATUS REPORT *)
let private computerStatusReport state =
    let fst = [
        "STATUS REPORT:"
        $"    KLINGONS LEFT: {state.TotalKlingons}"
        $"    MISSION MUST BE COMPLETED IN {double state.NumberOfStarDays - (state.StarDate - state.StartedOnStardate)} DAYS"
        ]

    if state.TotalStarbases > 0 then
        fst |> List.append [$"    THE FEDERATION IS MAINTAINING {state.TotalStarbases} STARBASES IN THE GALAXY"]
    else
        fst |> List.append ["    YOUR STUPIDITY HAS LEFT YOU ON YOUR ON IN THE GALAXY -- YOU HAVE NO STARBASES LEFT!"]
    
(* Menu option 2 PHOTON TORPEDO DATA *)
let private computerPhotonTorpedoData state =
    let makeCoordinates (klingon : Klingon) = 
        { 
            InitialX = double (fst state.CurrentSector + 1);
            InitialY = double (snd state.CurrentSector + 1);
            FinalX = double (fst klingon.SectorId + 1);
            FinalY = double (snd klingon.SectorId + 1)
        }

    getKlingons state |> List.map(fun klingon -> 
        [
            ""; 
            "FROM ENTERPRISE TO KLINGON BATTLE CRUISER "; 
            doDirectionCalculations  (makeCoordinates klingon); 
            doDistanceCalculations  (makeCoordinates klingon)
        ]
        ) |> List.concat 

 (* Menu option 3 STARBASE NAV DATA *)
let private computerStarbaseData state =
    let makeCoordinates state (starbase : Starbase) = 
        { 
            InitialX = double (fst state.CurrentSector + 1);
            InitialY = double (snd state.CurrentSector + 1);
            FinalX = double (fst starbase.SectorId + 1);
            FinalY = double (snd starbase.SectorId + 1)
        }

    let starbases = getStarbases state

    if starbases.Length = 0 then
            [
                "MR. SPOCK REPORTS, 'SENSORS SHOW NO STARBASES IN THIS QUADRANT.'"
            ]
    else
        starbases |> List.map(fun starbase -> 
            let coords = makeCoordinates state starbase
            [
                "FROM ENTERPRISE TO STARBASE " 
                doDirectionCalculations coords;
                doDistanceCalculations coords
            ] 
            ) |> List.concat

(* Menu option 4 DIRECTION/DISTANCE CALCULATOR *)
let private directionDistanceCalculator state  =
    printfn "DIRECTION/DISTANCE CALCULATOR:"
    printfn $"YOU ARE AT QUADRANT {fst state.CurrentQuadrant + 1},{snd state.CurrentQuadrant + 1} SECTOR {fst state.CurrentSector + 1},{snd state.CurrentSector + 1}"
    printfn ""
    let initCoordinate = inputCoordinate "PLEASE ENTER INITIAL COORDINATES (X,Y): "
    let finalCoordinate = inputCoordinate "PLEASE ENTER FINAL COORDINATES (X,Y): "
    getDirectionDistance initCoordinate finalCoordinate
    
(* Menu option 5 GALACY MAP *)
let private galaxyMap state  = 
    let printLine i : string =
        let mutable line = "| "
        for j in quadrantRange do
            let name = quadrantNameAlt (i, j)
            line <- line +  $"{name} | "
        line

    let printQuadrants i =
        [
            printLine i
            "+-------------+-------------+-------------+-------------+-------------+-------------+-------------+-------------+"
        ]

    quadrantRange |> List.map(fun i ->  printQuadrants i) |> List.concat |> List.append  [
        ""
        $"CURRENT QUADRANT {(quadrantName state.CurrentQuadrant)}"
        "+-I-----------+-II----------+-III---------+-IV----------+-I-----------+-II----------+-III---------+-IV----------+"
    ] 
    
(* Menu option 6 NAVIGATION DIRECTIONS *)
let private validNav (state : State)  =
    [
        "NAVIGATION DIRECTIONS"
        "====================="
        "4   3   2"
        "  \ ' /"
        "5 - * - 1"
        "  / ' \ "
        "6   7   8"
        ""
    ]

let computer (state : State) =
    let menu =
        [
            { Key = "1"; Text = "STATUS REPORT"; Function = computerStatusReport; Exit = false }
            { Key = "2"; Text = "PHOTON TORPEDO DATA"; Function = computerPhotonTorpedoData; Exit = false }
            { Key = "3"; Text = "STARBASE NAV DATA"; Function = computerStarbaseData; Exit = false }
            { Key = "4"; Text = "DIRECTION/DISTANCE CALCULATOR"; Function = directionDistanceCalculator; Exit = false }
            { Key = "5"; Text = "GALAXY MAP"; Function = galaxyMap; Exit = false }
            { Key = "6"; Text = "NAVIGATION DIRECTIONS"; Function = validNav; Exit = false }
            { Key = "7"; Text = "EXIT LIBRARY-COMPUTER"; Function = (fun state -> [""]); Exit = true }
        ]

    let printList (lst : string list) =
        lst |> List.iter(fun line -> printfn "%s" line)

    let mutable isOk = true

    while isOk do
        let cmd = inputValidMenuOption2 "COMPUTER ACTIVE AND AWAITING COMMAND? " menu
        match cmd.Exit with
        | true -> isOk <- false; ();
        | false -> 
            let lst = cmd.Function state
            printList lst
            ()

    state