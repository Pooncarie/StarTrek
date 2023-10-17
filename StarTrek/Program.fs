(*
10 REM SUPER STARTREK - MAY 16,1978 - REQUIRES 24K MEMORY
30 REM
40 REM ****        **** STAR TREK ****        ****
50 REM **** SIMULATION OF A MISSION OF THE STARSHIP ENTERPRISE,
60 REM **** AS SEEN ON THE STAR TREK TV SHOW.
70 REM **** ORIGIONAL PROGRAM BY MIKE MAYFIELD, MODIFIED VERSION
80 REM **** PUBLISHED IN DEC'S "101 BASIC GAMES", BY DAVE AHL.
90 REM **** MODIFICATIONS TO THE LATTER (PLUS DEBUGGING) BY BOB
100 REM *** LEEDOM - APRIL & DECEMBER 1974,
110 REM *** WITH A LITTLE HELP FROM HIS FRIENDS . . .
120 REM *** COMMENTS, EPITHETS, AND SUGGESTIONS SOLICITED --
130 REM *** SEND TO:  R. C. LEEDOM
140 REM ***           WESTINGHOUSE DEFENSE & ELECTRONICS SYSTEMS CNTR.
150 REM ***           BOX 746, M.S. 338
160 REM ***           BALTIMORE, MD  21203
170 REM ***
180 REM *** CONVERTED TO MICROSOFT 8 K BASIC 3/16/78 BY JOHN GORDERS
190 REM *** LINE NUMBERS FROM VERSION STREK7 OF 1/12/75 PRESERVED AS
200 REM *** MUCH AS POSSIBLE WHILE USING MULTIPLE STATEMENTS PER LINE
205 REM *** SOME LINES ARE LONGER THAN 72 CHARACTERS; THIS WAS DONE
210 REM *** BY USING "?" INSTEAD OF "PRINT" WHEN ENTERING LINES
215 REM ***
*)

open System

type Command = NAV | SRS | LRS | PHA | TOR | SHE | DAM | COM | XXX | INV

type SectorId = int * int
type QuadrantId = int * int

type Sector = {
    Starbase : bool
    Star : bool
    Enterprise : bool
    Klingon : bool
    SectorId : SectorId
}

type Quadrant = {
    Sectors : Sector array2d
    Starbases : int
    Stars : int
    Klingons : int
    QuadrantId : QuadrantId
}

type Galaxy = {
    Quadrants : Quadrant array2d 
    }

type State = {
    Galaxy : Galaxy option
    StarDate : int
    Condition : string
    CurrentQuadrant : QuadrantId
    CurrentSector : SectorId
    Torpedoes : int
    TotalEnergy : int
    Klingons : int
    ShortRangeScanners : bool
    LongRangerScanners : bool
    Energy : int
    Shields : int
}

let klingonShip = "+K+"
let starbase = "<*>"
let star = " * "
let space = "   "
let enterprise = " E "



let sectorCreate x y = 
    { Starbase = false; Star = false; Klingon = false; Enterprise = false; SectorId = (x, y) }
    

let quadrantCreate x y =
    let klingonCount() =
        let rnd = Random();
        match rnd.NextDouble() with
            | x when x > 0.98 -> 3
            | x when x > 0.95 -> 2
            | x when x > 0.80 -> 1
            | _ -> 0

    let starBaseCount() =
        let rnd = Random();
        match rnd.NextDouble() with
            | x when x > 0.96-> 1
            | _ -> 0

    let starCount() =
        let rnd = Random();
        match rnd.NextDouble() with
            | x when x > 0.80 -> 1
            | _ -> 0

    let quadrant = { 
        Starbases = starBaseCount()
        Stars = starCount()
        Klingons = klingonCount()
        QuadrantId = (x, y)
        Sectors = Array2D.init 8 8 (fun i j -> sectorCreate i j) 
    }

    quadrant

let createGalaxy = { Quadrants = Array2D.init 8 8 (fun i j -> quadrantCreate i j) }

let createState = {
    Galaxy = Some createGalaxy
    StarDate = 0
    Condition = "GREEN"
    CurrentQuadrant = QuadrantId(0, 0)
    CurrentSector = SectorId(0,0)
    Torpedoes = 10
    TotalEnergy = 3000
    Klingons = 15
    ShortRangeScanners = true
    LongRangerScanners = true
    Energy = 1000
    Shields = 100
}

let quadrantName state =
        let partNames = [" I"; " II"; " III"; " IV";]
        if (snd state.CurrentQuadrant) < 4 then
            match fst state.CurrentQuadrant with
            | 0 -> "ANTARES" + partNames.[snd state.CurrentQuadrant % 4]
            | 1 -> "RIGEL" + partNames.[snd state.CurrentQuadrant % 4]
            | 2 -> "PROCYON" + partNames.[snd state.CurrentQuadrant % 4]
            | 3 -> "VEGA" + partNames.[snd state.CurrentQuadrant % 4]
            | 4 -> "CANOPUS" + partNames.[snd state.CurrentQuadrant % 4]
            | 5 -> "ALTAIR" + partNames.[snd state.CurrentQuadrant % 4]
            | 6 -> "SAGITTARIUS" + partNames.[snd state.CurrentQuadrant % 4]
            | 7 -> "POLLUX" + partNames.[snd state.CurrentQuadrant % 4]
            | _ -> ""
        else
            match fst state.CurrentQuadrant with
            | 0 -> "SIRIUS" + partNames.[snd state.CurrentQuadrant % 4]
            | 1 -> "DENEB" + partNames.[snd state.CurrentQuadrant % 4]
            | 2 -> "CAPELLA" + partNames.[snd state.CurrentQuadrant % 4]
            | 3 -> "BETELGEUSE" + partNames.[snd state.CurrentQuadrant % 4]
            | 4 -> "ALDEBARAN" + partNames.[snd state.CurrentQuadrant % 4]
            | 5 -> "REGULUS" + partNames.[snd state.CurrentQuadrant % 4]
            | 6 -> "ARCTURUS" + partNames.[snd state.CurrentQuadrant % 4]
            | 7 -> "SPICA" + partNames.[snd state.CurrentQuadrant % 4]
            | _ -> ""

let start() =
    [0..10] |> List.iter(fun x -> printfn "")
    printfn "                                    ,------*------,"
    printfn "                    ,-------------   '---  ------'"
    printfn "                     '-------- --'      / /"
    printfn "                         ,---' '-------/ /--,"
    printfn "                          '----------------'"
    printfn "                    THE USS ENTERPRISE --- NCC-1701"
    printfn ""
    printfn ""
    ()

let printSector state line =
    let sector = state.Galaxy.Value.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant].Sectors;

    let printIt s =
        if s.Enterprise then
            printf " %s" enterprise
        elif s.Klingon then
            printf " %s" klingonShip
        elif s.Starbase then
            printf " %s" starbase
        elif s.Star then
            printf " %s" star
        else
            printf " %s" space

    for i in [0..7] do 
        printIt sector.[line, i]
    done


let shortRangeScan state = 
    printfn "SHORT RANGE SCAN FOR QUADRANT %s" (quadrantName state)
    printfn "   "
    printfn " +--1---2---3---4---5---6---7---8-+"
    printf "1|"
    printSector state 0; printf "|1";
    printfn $"        STARDATE            {state.StarDate}"
    printf "2|"
    printSector state 1; printf "|2";
    printfn $"        CONDITION           {state.Condition}"
    printf "3|"
    printSector state 2; printf "|3";
    printfn $"        QUADRANT            {state.CurrentQuadrant}"
    printf "4|"
    printSector state 3; printf "|4";
    printfn $"        SECTOR              {state.CurrentSector}"
    printf "5|"
    printSector state 4; printf "|5";
    printfn $"        PHOTO TORPEDOES     {state.Torpedoes}"
    printf "6|"
    printSector state 5; printf "|6";
    printfn $"        TOTAL ENERGY        {state.Energy}"
    printf "7|"
    printSector state 6; printf "|7";
    printfn $"        SHIELDS             {state.Shields}"
    printf "8|"
    printSector state 7; printf "|8";
    printfn $"        KLINGONS REMAINING  {state.Klingons}"
    printf " +--------------------------------+"
    printfn "   "

let beginMission state =
    printfn "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED"
    printfn "IN THE GALACTIC QUADRANT %s" (quadrantName state)
    shortRangeScan state

let command() =
    let getCommand() =
        printfn "   "
        printfn "Enter one of the following commands:"
        printfn "NAV - TO SET COURSE"
        printfn "SRS - FOR SHORT RANGE SENSOR SCAN"
        printfn "LRS - FOR LONG RANGE SENSOR SCAN"
        printfn "PHA - TO FIRE PHASERS"
        printfn "TOR - TO FIRE PHOTON TORPEDOES"
        printfn "SHE - FOR SHIELD CONTROL"
        printfn "DAM - TO GET DAMAGE REPORTS"
        printfn "COM - TO CALL ON LIBRARY-COMPUTER"
        printfn "XXX - TO RESIGN YOUR COMMAND"
        printfn "   "
        printf "COMMAND ? "
        Console.ReadLine()

    let mutable validCommand = false;
    let mutable command = Command.INV;

    while not validCommand do
        let cmd = getCommand()
        match cmd.ToUpper() with
        | "NAV" -> validCommand <- true; command <- Command.NAV;
        | "SRS" -> validCommand <- true; command <- Command.SRS;
        | "LRS" -> validCommand <- true; command <- Command.LRS;
        | "PHA" -> validCommand <- true; command <- Command.PHA;
        | "TOR" -> validCommand <- true; command <- Command.TOR;
        | "SHE" -> validCommand <- true; command <- Command.SHE;
        | "DAM" -> validCommand <- true; command <- Command.DAM;
        | "COM" -> validCommand <- true; command <- Command.COM;
        | "XXX" -> validCommand <- true; command <- Command.XXX;
        | _ -> printfn "Invalid command"; 
        
    command

let mutable state = createState

let changeQuadrant state =
    let rnd = Random();
    let newQuadrant = (rnd.Next(0, 8), rnd.Next(0, 8))
    let s1 = { state with CurrentQuadrant = newQuadrant }
    let newSector = (rnd.Next(0, 8), rnd.Next(0, 8))
    { s1 with CurrentSector = newSector}

let nav() =
    printfn "NAV"

let srs() =
    printfn "SRS"

let lrs state =
    if not state.LongRangerScanners then
        printfn "LONG RANGE SENSORS ARE INOPERABLE."
    else
        printfn $"LONG RANGE SCAN FOR QUADRANT {quadrantName state}"

let pha() =
    printfn "PHA"

let tor() =
    printfn "TOR"

let she() =
    printfn "SHE"

let dam() =
    printfn "DAM"

let com() =
    printfn "COM"

let xxx() =
    printfn "XXX"

let rec mainLoop state =
    let command = command()
    match command with
    | Command.NAV -> nav()
    | Command.SRS -> srs()
    | Command.LRS -> lrs state
    | Command.PHA -> pha()
    | Command.TOR -> tor()
    | Command.SHE -> she()
    | Command.DAM -> dam()
    | Command.COM -> com()
    | Command.XXX -> xxx()
    | _ -> printfn "Invalid command"

    mainLoop state

start()

state <- changeQuadrant state
beginMission state

mainLoop state
