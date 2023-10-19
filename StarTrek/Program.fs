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
    CurrentStarDate : int
    Condition : string
    CurrentQuadrant : QuadrantId
    CurrentSector : SectorId
    Torpedoes : int
    TotalEnergy : int
    TotalKlingons : int
    TotalStarbases : int
    TotalStars : int
    ShortRangeScanners : bool
    LongRangerScanners : bool
    Energy : int
    Shields : int
    DirectionArray : int array2d
    }


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
            | x when x > 0.96 -> 1
            | _ -> 0

    let starCount() =
        let rnd = Random();
        int  (rnd.NextDouble() * 7.98 + 1.01)

    let quadrant = { 
        Starbases = starBaseCount()
        Stars = starCount()
        Klingons = klingonCount()
        QuadrantId = (x, y)
        Sectors = Array2D.init 8 8 (fun i j -> sectorCreate i j) 
    }

    quadrant

let createGalaxy = { Quadrants = Array2D.init 8 8 (fun i j -> quadrantCreate i j) }

let createState = 
    let arrayOfMove = [|
            [| 1; 0 |]
            [| 1; -1 |]
            [| 0; -1 |]
            [| -1; 1 |]
            [| -1; -0 |]
            [| -1; 1 |]
            [| 0; 1 |]
            [| 1; 1 |]
            [| 1; 0|]
        |]

    let s = {
        Galaxy = Some createGalaxy
        StarDate = 0
        CurrentStarDate = 0
        Condition = "GREEN"
        CurrentQuadrant = QuadrantId(0, 0)
        CurrentSector = SectorId(0,0)
        Torpedoes = 10
        TotalEnergy = 3000
        TotalKlingons = 0
        TotalStarbases = 0
        TotalStars = 0
        ShortRangeScanners = true
        LongRangerScanners = true
        Energy = 1000
        Shields = 100
        DirectionArray = Array2D.init 9 2 (fun i j -> arrayOfMove[i][j])
        }

    let mutable totalStarbases = 0
    let mutable totalKlingons = 0
    let mutable totalStars = 0

    for i in [0..7] do
        for j in [0..7] do
            let quadrant = s.Galaxy.Value.Quadrants[i, j]
            totalStarbases <- totalStarbases + quadrant.Starbases
            totalKlingons <- totalKlingons + quadrant.Klingons
            totalStars <- totalStars + quadrant.Stars

    let rnd = Random();
    let starDate = 25 + (int) (rnd.NextDouble() * 10.0)

    { s with TotalKlingons = totalKlingons; TotalStarbases = totalStarbases; TotalStars = totalStars; StarDate = starDate}


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
    let klingonShip = "+K+"
    let starbase = "<*>"
    let star = " * "
    let space = "   "
    let enterprise = " E "

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
    printfn $"        STARDATE            {state.CurrentStarDate}"
    printf "2|"
    printSector state 1; printf "|2";
    printfn $"        CONDITION           {state.Condition}"
    printf "3|"
    printSector state 2; printf "|3";
    printfn $"        QUADRANT            {fst state.CurrentQuadrant + 1},{snd state.CurrentQuadrant + 1}"
    printf "4|"
    printSector state 3; printf "|4";
    printfn $"        SECTOR              {fst state.CurrentSector + 1},{snd state.CurrentSector + 1}"
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
    printfn $"        KLINGONS REMAINING  {state.TotalKlingons}"
    printf " +--------------------------------+"
    printfn "   "

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

let changeQuadrant state newQuadrant =
    let rnd = Random();
    let sectorIndexs = Seq.initInfinite (fun _ -> (rnd.Next(0, 8), rnd.Next(0, 8))) |> Seq.distinct |> Seq.take(30) |> Seq.toArray

    let s1 = { state with CurrentQuadrant = newQuadrant }
    let quadrant = s1.Galaxy.Value.Quadrants.[fst newQuadrant, snd newQuadrant]

    let mutable cnt = 0

    state.Galaxy.Value.Quadrants.[fst s1.CurrentQuadrant, snd s1.CurrentQuadrant].Sectors.[fst sectorIndexs[0], snd sectorIndexs[0]]
        <- { quadrant.Sectors.[fst sectorIndexs[cnt], snd sectorIndexs[cnt]] with Enterprise = true }

    cnt <- cnt + 1

    [0..quadrant.Klingons - 1] |> List.iter(fun x -> 
        let newKlingonSector = sectorIndexs[x + cnt]
        state.Galaxy.Value.Quadrants.[fst s1.CurrentQuadrant, snd s1.CurrentQuadrant].Sectors.[fst newKlingonSector, snd newKlingonSector] 
            <- { quadrant.Sectors.[fst newKlingonSector, snd newKlingonSector] with Klingon = true }
        cnt <- cnt + 1
    )

    [0..quadrant.Starbases - 1] |> List.iter(fun x -> 
        let newStarbaseSector = sectorIndexs[x + cnt]
        state.Galaxy.Value.Quadrants.[fst s1.CurrentQuadrant, snd s1.CurrentQuadrant].Sectors.[fst newStarbaseSector, snd newStarbaseSector] 
            <- { quadrant.Sectors.[fst newStarbaseSector, snd newStarbaseSector] with Starbase = true }
        cnt <- cnt + 1
    )

    [0..quadrant.Stars - 1] |> List.iter(fun x -> 
        let newStarSector = sectorIndexs[x + cnt]
        state.Galaxy.Value.Quadrants.[fst s1.CurrentQuadrant, snd s1.CurrentQuadrant].Sectors.[fst newStarSector, snd newStarSector] 
            <- { quadrant.Sectors.[fst newStarSector, snd newStarSector] with Star = true }
        cnt <- cnt + 1
    )

    printfn ""
    printfn $"NOW ENTERING {quadrantName state} QUADRANT . . ."
    printfn ""
    { s1 with CurrentSector = sectorIndexs[0]}


let startGame (state : State) =
    (* Start in a random quadrant *)
    let rnd = Random();
    let newQuadrant = (rnd.Next(0, 8), rnd.Next(0, 8))
    let s1 = changeQuadrant state newQuadrant

    printfn "YOUR ORDERS ARE AS FOLLOWS:"
    printfn "   DESTROY THE %d KLINGON WARSHIPS WHICH HAVE INVADED" state.TotalKlingons
    printfn "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS"
    printfn "   ON STARDATE %d. THIS GIVES YOU %d DAYS. THERE ARE %d" state.StarDate (state.StarDate - state.CurrentStarDate) state.TotalStarbases
    printfn "   STARBASES IN THE GALAXY FOR RESUPPLYING YOUR SHIP."
    printfn ""
    printfn ""
    printfn "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED"
    printfn "IN THE GALACTIC QUADRANT %s" (quadrantName state)
    s1


let nav state =
    let getCourse() =
        printfn "COURSE (0-9) "
        let mutable str = Console.ReadLine().Trim()
        let mutable num = 0
        let mutable ok = false

        while ok = false do
            while Int32.TryParse(str, &num) = false do
                printfn "   LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"; printfn "";
                printfn "COURSE (0-9) "
                str <- Console.ReadLine().Trim()
           
            if (num < 1 || num > 9) then
                printfn "   LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"; printfn "";
                printfn "COURSE (0-9) "
                str <- Console.ReadLine().Trim()
            else
                ok <- true

        if num = 9 then
            num <- 0
        else
            num <- num - 1

    // TODO: warp factor is dependant on energy ????
    let getWarpFactor() =
        printfn "WARP FACTOR (0-8) "
        let mutable str = Console.ReadLine().Trim()
        let mutable num = 0
        let mutable ok = false

        while ok = false do
            while Int32.TryParse(str, &num) = false do
                printfn "   CHIEF ENGINEER SCOTT REPORTS, 'THE ENGINES WON'T TAKE"
                printfn $"   WARP {num}!'"; printfn "";
                printfn "WARP FACTOR (0-8) "
                str <- Console.ReadLine().Trim()
           
            if (num < 0 || num > 8) then
                printfn "   CHIEF ENGINEER SCOTT REPORTS, 'THE ENGINES WON'T TAKE"
                printfn $"   WARP {num}!'"; printfn "";
                printfn "WARP FACTOR (0-8) "
                str <- Console.ReadLine().Trim()
            else
                ok <- true
        num

    let course = getCourse()
    let warpFactor = getWarpFactor()

    let rnd = Random();
    let newQuadrant = (rnd.Next(0, 8), rnd.Next(0, 8))

    let s1 = changeQuadrant state newQuadrant

    s1

let longRangeScan state =
    printfn "LONG RANGE SCAN FOR QUADRANT %s" (quadrantName state)
    printfn "   "
    printfn "-------------------------------"


let srs state =
    shortRangeScan state
    state

let lrs state =
    if not state.LongRangerScanners then
        printfn "LONG RANGE SENSORS ARE INOPERABLE."
    else
        printfn $"LONG RANGE SCAN FOR QUADRANT {quadrantName state}"
    state

let pha state =
    printfn "PHA"
    state

let tor state =
    printfn "TOR"
    state

let she state =
    printfn "SHE"
    state

let dam state =
    printfn "DAM"
    state

let com state =
    printfn "COM"
    state

let xxx state =
    printfn "XXX"
    state

let rec mainLoop st =
    let command = command()
    let mutable state = st
    state <- match command with
             | Command.NAV -> nav state
             | Command.SRS -> srs state
             | Command.LRS -> lrs state
             | Command.PHA -> pha state
             | Command.TOR -> tor state
             | Command.SHE -> she state
             | Command.DAM -> dam state
             | Command.COM -> com state
             | Command.XXX -> xxx state
             | _ -> printfn "Invalid command"; state

    mainLoop state

start()


let mutable state = createState

state <- startGame state

mainLoop state
