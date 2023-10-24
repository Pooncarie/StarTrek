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
    TotalStardate : int
    Condition : string
    CurrentQuadrant : QuadrantId
    CurrentSector : SectorId
    Torpedoes : int
    TotalEnergy : int
    TotalKlingons : int
    TotalStarbases : int
    TotalStars : int
    EngineDamage : int                  // D(1)
    SRS_Damage : int                    // D(2)
    LRS_Damage : int                    // D(3)
    PhasersDamage : int                 // D(4) 
    DeflectorDamage : int               // D(7)
    ComputerDamage : int                // D(8)
    Energy : int
    ShieldEnergy : int
    DirectionArray : int array2d
    StartAgain : bool
    Error : bool
    }

let readLine() = Console.ReadLine().Trim().ToUpper();

let rnd = Random();

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

let quadrantCreate x y =

    let klingonCount() =
        match rnd.NextDouble() with
            | x when x > 0.98 -> 3
            | x when x > 0.95 -> 2
            | x when x > 0.80 -> 1
            | _ -> 0

    let starBaseCount() =
        match rnd.NextDouble() with
            | x when x > 0.96 -> 1
            | _ -> 0

    let starCount() =
        int  (rnd.NextDouble() * 7.98 + 1.01)

    let sectorCreate x y = 
        { Starbase = false; Star = false; Klingon = false; Enterprise = false; SectorId = (x, y) }

    let quadrant = { 
        Starbases = starBaseCount()
        Stars = starCount()
        Klingons = klingonCount()
        QuadrantId = (x, y)
        Sectors = Array2D.init 8 8 (fun i j -> sectorCreate i j) 
    }

    quadrant


let createState = 
    let createGalaxy = { Quadrants = Array2D.init 8 8 (fun i j -> quadrantCreate i j) }

    let arrayOfMove = [|
            [| -1; 0 |]    
            [| -1; -1 |]   
            [| 0; -1 |]    
            [| 1; -1 |]    
            [| 1; 0 |]     
            [| 1; 1 |]     
            [| 0; 1|]      
            [| -1; 1 |]    
        |]

    let s = {
        Galaxy = Some createGalaxy
        StarDate = 0
        CurrentStarDate = 0
        TotalStardate = 0
        Condition = "GREEN"
        CurrentQuadrant = QuadrantId(0, 0)
        CurrentSector = SectorId(0,0)
        Torpedoes = 10
        TotalEnergy = 3000
        TotalKlingons = 0
        TotalStarbases = 0
        TotalStars = 0
        EngineDamage = 0
        SRS_Damage = 0
        LRS_Damage = 0
        PhasersDamage = 0
        ComputerDamage = 0
        DeflectorDamage = 0
        Energy = 1000
        ShieldEnergy = 100
        DirectionArray = Array2D.init 8 2 (fun i j -> arrayOfMove[i][j])
        StartAgain = false;
        Error = false;
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

    let totalStarDate = 25 + (int) (rnd.NextDouble() * 10.0)
    (* T  & T0 *)
    let starDate = int (rnd.NextDouble() * 20.0 + 20.0) * 100

    { s with TotalKlingons = totalKlingons; TotalStarbases = totalStarbases; TotalStars = totalStars; StarDate = starDate; TotalStardate = totalStarDate; CurrentStarDate = starDate; }


let quadrantName quadrant =
        let partNames = [" I"; " II"; " III"; " IV";]

        if (snd quadrant) < 4 then
            match fst quadrant with
            | 0 -> "ANTARES" + partNames.[snd quadrant % 4]
            | 1 -> "RIGEL" + partNames.[snd quadrant % 4]
            | 2 -> "PROCYON" + partNames.[snd quadrant % 4]
            | 3 -> "VEGA" + partNames.[snd quadrant % 4]
            | 4 -> "CANOPUS" + partNames.[snd quadrant % 4]
            | 5 -> "ALTAIR" + partNames.[snd quadrant % 4]
            | 6 -> "SAGITTARIUS" + partNames.[snd quadrant % 4]
            | 7 -> "POLLUX" + partNames.[snd quadrant % 4]
            | _ -> ""
        else
            match fst quadrant with
            | 0 -> "SIRIUS" + partNames.[snd quadrant % 4]
            | 1 -> "DENEB" + partNames.[snd quadrant % 4]
            | 2 -> "CAPELLA" + partNames.[snd quadrant % 4]
            | 3 -> "BETELGEUSE" + partNames.[snd quadrant % 4]
            | 4 -> "ALDEBARAN" + partNames.[snd quadrant % 4]
            | 5 -> "REGULUS" + partNames.[snd quadrant % 4]
            | 6 -> "ARCTURUS" + partNames.[snd quadrant % 4]
            | 7 -> "SPICA" + partNames.[snd quadrant % 4]
            | _ -> ""

let quadrantNameAlt quadrant =
        if (snd quadrant) < 4 then
            match fst quadrant with
            | 0 -> "ANTARES    "
            | 1 -> "RIGEL      "
            | 2 -> "PROCYON    "
            | 3 -> "VEGA       "
            | 4 -> "CANOPUS    "
            | 5 -> "ALTAIR     "
            | 6 -> "SAGITTARIUS"
            | 7 -> "POLLUX     "
            | _ -> ""
        else
            match fst quadrant with
            | 0 -> "SIRIUS     "
            | 1 -> "DENEB      "
            | 2 -> "CAPELLA    "
            | 3 -> "BETELGEUSE "
            | 4 -> "ALDEBARAN  "
            | 5 -> "REGULUS    "
            | 6 -> "ARCTURUS   "
            | 7 -> "SPICA      "
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
    

let printSector state line =
    let klingonShip = "+K+"
    let starbase = ">!<"
    let star = " * "
    let space = "   "
    let enterprise = "<E>"

    let printIt s =
        match s with
        | { Starbase = true; } -> printf $" {starbase}"
        | { Enterprise = true; } -> printf $" {enterprise}"
        | { Klingon = true; } -> printf $" {klingonShip}"
        | { Star = true;  } -> printf $" {star}"
        | _ -> printf $" {space}"

    let sector = state.Galaxy.Value.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant].Sectors;
    for i in [0..7] do 
        printIt sector.[line, i]
    done


let shortRangeScan state = 
    printfn $"SHORT RANGE SCAN FOR QUADRANT {(quadrantName state.CurrentQuadrant)}"
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
    printfn $"        SHIELDS             {state.ShieldEnergy}"
    printf "8|"
    printSector state 7; printf "|8";
    printfn $"        KLINGONS REMAINING  {state.TotalKlingons}"
    printf " +--------------------------------+"
    printfn "   "
    state


let changeQuadrant state newQuadrant : State =
    let clearSectors quadrant = 
        for i in [0..7] do
            for j in [0..7] do
                quadrant.Sectors.[i, j]
                    <- { quadrant.Sectors.[i, j] with Enterprise = false; Klingon = false;  Starbase = false; Star = false }

    let sectorIndexs = Seq.initInfinite (fun _ -> (rnd.Next(0, 7), rnd.Next(0, 7))) |> Seq.distinct |> Seq.take(40) |> Seq.toArray
    let quadrant = state.Galaxy.Value.Quadrants.[fst newQuadrant, snd newQuadrant]

    clearSectors quadrant

    let mutable cnt = 0

    (* 
        TODO: Currently putting Enterprize into a new sector BUT I think it should be in the sector the user 
        has navigated to. This will is ok for the moment.
    *)
    quadrant.Sectors.[fst sectorIndexs[0], snd sectorIndexs[0]]
        <- { quadrant.Sectors.[fst sectorIndexs[cnt], snd sectorIndexs[cnt]] with Enterprise = true }

    cnt <- cnt + 1

    // TODO: THIS IS JUST FOR TESTING
    //quadrant.Sectors.[fst sectorIndexs[cnt], snd sectorIndexs[cnt]]
    //    <- { quadrant.Sectors.[fst sectorIndexs[cnt], snd sectorIndexs[cnt]] with Starbase = true }
    //cnt <- cnt + 1

    [0..quadrant.Klingons - 1] |> List.iter(fun x -> 
        let newKlingonSector = sectorIndexs[x + cnt]
        quadrant.Sectors.[fst newKlingonSector, snd newKlingonSector] 
            <- { quadrant.Sectors.[fst newKlingonSector, snd newKlingonSector] with Klingon = true }
        cnt <- cnt + 1
    )

    [0..quadrant.Starbases - 1] |> List.iter(fun x -> 
        let newStarbaseSector = sectorIndexs[x + cnt]
        quadrant.Sectors.[fst newStarbaseSector, snd newStarbaseSector] 
            <- { quadrant.Sectors.[fst newStarbaseSector, snd newStarbaseSector] with Starbase = true }
        cnt <- cnt + 1
    )

    [0..quadrant.Stars - 1] |> List.iter(fun x -> 
        let newStarSector = sectorIndexs[x + cnt]
        quadrant.Sectors.[fst newStarSector, snd newStarSector] 
            <- { quadrant.Sectors.[fst newStarSector, snd newStarSector] with Star = true }
        cnt <- cnt + 1
    )

    printfn ""
    printfn $"NOW ENTERING {quadrantName newQuadrant} QUADRANT . . ."
    if quadrant.Klingons > 0 then
        printfn "COMBAT AREA **** CONDITION RED"
        { state with CurrentSector = sectorIndexs[0]; CurrentQuadrant = newQuadrant; Condition = "*RED*" }
    else
        { state with CurrentSector = sectorIndexs[0]; CurrentQuadrant = newQuadrant; Condition = "GREEN";   }


let startGame (state : State) =
    (* Start in a random quadrant *)
    let newQuadrant = (rnd.Next(0, 8), rnd.Next(0, 8))
    let newState = changeQuadrant state newQuadrant

    printfn "YOUR ORDERS ARE AS FOLLOWS:"
    printfn $"   DESTROY THE {state.TotalKlingons} KLINGON WARSHIPS WHICH HAVE INVADED" 
    printfn "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS"
    printfn $"   ON STARDATE {state.StarDate}. THIS GIVES YOU {(state.StarDate - state.CurrentStarDate)} DAYS. THERE ARE {state.TotalStarbases}"   
    printfn "   STARBASES IN THE GALAXY FOR RESUPPLYING YOUR SHIP."
    printfn ""
    printfn ""
    printfn "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED"
    printfn $"IN THE GALACTIC QUADRANT {(quadrantName newState.CurrentQuadrant)}"
    newState


(* Show all the quadrant names, mostly for debugging purposes. *)
let galaxyMap(quadrant) = 
    printfn $"CURRENT QUADRANT {(quadrantName quadrant)}"
    printfn "+-I-----------+-II----------+-III---------+-IV----------+-I-----------+-II----------+-III---------+-IV----------+"
    for i in [0..7] do
        printf "| "
        for j in [0..7] do
            let name = quadrantNameAlt (i, j)
            printf $"{name} | "
        printfn ""
        printfn "+-------------+-------------+-------------+-------------+-------------+-------------+-------------+-------------+"

let computerStatusReport state =
    printfn "STATUS REPORT:"
    printfn $"    KLINGONS LEFT: {state.TotalKlingons}"
    printfn $"    MISSION MUST BE COMPLETED IN {int (0.1 * double((state.CurrentStarDate + state.TotalStardate - state.StarDate) * 10)) } STARDATES"
    if state.TotalStarbases > 0 then
        printfn $"    THE FEDERATION IS MAINTAINING {state.TotalStarbases} STARBASES IN THE GALAXY"
    else
        printfn "    YOUR STUPIDITY HAS LEFT YOU ON YOUR ON IN THE GALAXY -- YOU HAVE NO STARBASES LEFT!"

let getCoordinates(state : State) =
    printfn "DIRECTION/DISTANCE CALCULATOR:"
    printfn $"YOU ARE AT QUADRANT {fst state.CurrentQuadrant + 1},{snd state.CurrentQuadrant + 1} SECTOR {fst state.CurrentSector + 1},{snd state.CurrentSector + 1}"
    printf "PLEASE ENTER INITIAL COORDINATES (X,Y)"
    let initialCoordinate = readLine
    printf "FINAL COORDINATES (X,Y)"
    let finalCoordinate = readLine
    ()
    
(* 
    C1 -> fst initialCoordinate 
    A -> snd initialCoordinate
    W1 -> fst finalCoordinate
    X -> snd finalCoordinate
*)
let distanceCalculator (initialCoordinate : (int * int)) (finalCoordinate : (int * int)) = 
    let x = snd finalCoordinate - snd initialCoordinate
    let a = fst initialCoordinate - fst finalCoordinate
    if x >= 0 then
        if a < 0 then
            let c1 = 7
            if abs a < abs x then
                printfn $"DIRECTION = {c1 + (( ((abs x) - (abs a)) + (abs x)) / (abs x))} "
            else
                printfn $"DIRECTION = {c1 + ((abs x) / (abs a))}"
            printfn $"DISTANCE = {sqrt (double x * double x + double a * double a)}"
    0

let computerPhotonTorpedoData(state : State) =
    let findKlingon (arr: Sector [,]) = 
        let rec go x y =
          if y >= arr.GetLength 1 then None
          elif x >= arr.GetLength 0 then go 0 (y+1)
          elif arr.[x,y].Klingon = true then Some (x,y)
          else go (x+1) y
        
        go 0 0

    let quadrant = state.Galaxy.Value.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant];
    if quadrant.Klingons > 0 then
        printfn "FROM ENTERPRISE TO KLINGON BATTLE CRUISER"
        let initialCoordinate = state.CurrentSector
        let finalCoordinate = findKlingon quadrant.Sectors
        match finalCoordinate with
        | None -> printfn "NO KLINGON IN THIS QUADRANT"; 
        | Some finalCoordinate -> printfn $"Distance {distanceCalculator initialCoordinate finalCoordinate}";

    ()

let getCourse() : int option =
    let courseError() =
        printfn "   LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"; 
        printfn "";

    let course = (inputInteger "COURSE (0-8) ") - 1

    if course < 0 || course > 7 then
        courseError()
        None
    else
        Some course

let getWarp state : int option =
    let warpError warpSpeed =
        printfn "   CHIEF ENGINEER SCOTT REPORTS, 'THE ENGINES WON'T TAKE"
        printfn $"   WARP {warpSpeed}!'"; 
        printfn "";

    let warpRange = 
        if state.EngineDamage < 0 then
            "WARP FACTOR (0-0.2) "
        else
            "WARP FACTOR (0-8) "

    let warpFactor = inputDouble warpRange

    if state.EngineDamage < 0 && warpFactor > 0.2 then
        printfn "WARP ENGINES ARE DAMAGED.  MAXIUM SPEED = WARP 0.2"
        None
    else
        if warpFactor > 0 && warpFactor <= 8 then
            let n = int (warpFactor * 8.0 + 0.5)
            if state.Energy - n >= 0 then
                Some n
            else
                printfn "ENGINEERING REPORTS   'INSUFFICIENT ENERGY AVAILABLE"
                printfn $"                     FOR MANEUVERING AT WARP {warpFactor}!'"
               
                if state.ShieldEnergy < n - state.Energy || state.DeflectorDamage < 0 then
                    None
                else
                    printfn $"DEFLECTOR CONTROL ROOM ACKNOWLEDGES  {state.ShieldEnergy} UNITS OF ENERGY"
                    printfn "                         PRESENTLY DEPLOYED TO SHIELDS."
                    None
        else 
            if warpFactor = 0.0 then
                None
            else
                warpError warpFactor
                None

let navigateQuadrant state x y : State =
    if fst state.CurrentQuadrant + x < 0 || fst state.CurrentQuadrant + x > 7 || snd state.CurrentQuadrant + y < 0 || snd state.CurrentQuadrant + y > 7 then
        printfn "LT. UHURA REPORTS MESSAGE FROM STARFLEET COMMAND:"
        printfn "  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER"
        printfn "  IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.'"
        printfn "CHIEF ENGINEER SCOTT REPORTS  'WARP ENGINES SHUT DOWN"
        printfn $"AT SECTOR {fst state.CurrentSector + 1},{snd state.CurrentSector + 1} OF QUADRANT {fst state.CurrentQuadrant + 1},{snd state.CurrentQuadrant + 1}"
        { state with Error = true;}
    else
        let newQuadrant = (fst state.CurrentQuadrant + x, snd state.CurrentQuadrant + y)
        
        changeQuadrant state newQuadrant

let navigateSector state course : State =
    let x1 = state.DirectionArray[course,0]
    let x2 = state.DirectionArray[course,1]

    if fst state.CurrentSector + x1 < 0 || fst state.CurrentSector + x1 > 7 || snd state.CurrentSector + x2 < 0 || snd state.CurrentSector + x2 > 7 then
        navigateQuadrant state x1 x2
    else
        let quadrant = state.Galaxy.Value.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant]
        let sector = quadrant.Sectors.[fst state.CurrentSector + x1, snd state.CurrentSector + x2]

        if sector.Klingon || sector.Star then
             printfn $"WARP ENGINES SHUT DOWN AT SECTOR {fst state.CurrentSector + 1},{snd state.CurrentSector + 1} DUE TO BAD NAVIGATION"
             { state with Error = true }
        else if sector.Starbase then
            (* we have docked with starbase *)
            printfn " SHIELDS DROPPED FOR DOCKING PURPOSES"
            for i in [0..7] do
                for j in [0..7] do
                    if quadrant.Sectors.[i, j].Enterprise then
                        quadrant.Sectors.[i, j] <- { quadrant.Sectors.[i, j] with Enterprise = false }
                    if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                        quadrant.Sectors.[i, j] <- { quadrant.Sectors.[i, j] with Enterprise = true }
            { state with ShieldEnergy = 0; Condition = "DOCKED"; CurrentSector = (fst state.CurrentSector + x1, snd state.CurrentSector + x2) }
        else    
            for i in [0..7] do
                for j in [0..7] do
                    if quadrant.Sectors.[i, j].Enterprise then
                        quadrant.Sectors.[i, j] <- { quadrant.Sectors.[i, j] with Enterprise = false }
                    if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                        quadrant.Sectors.[i, j] <- { quadrant.Sectors.[i, j] with Enterprise = true }

            { state with CurrentSector = (fst state.CurrentSector + x1, snd state.CurrentSector + x2); }


let navigate state =
    let course = getCourse()
    
    match course with
    | None -> state
    | Some course ->
        let warpSpeed = getWarp state
        printfn ""

        match warpSpeed with
        | None -> state
        | Some warpSpeed ->
            let mutable s1 = state
            for i = 0 to warpSpeed - 1 do
                if s1.Error = false then
                    s1 <- navigateSector s1 course
            shortRangeScan { s1 with Error = false;}


let longRangeScan state =
    let getQuadrant (state : State) (qadrantId : (int * int)) : Quadrant option =
        if (fst qadrantId) < 0 || (fst qadrantId) > 7 || (snd qadrantId) < 0 || (snd qadrantId) > 7 then
            None
        else
            Some state.Galaxy.Value.Quadrants.[fst qadrantId, snd qadrantId]

    if state.LRS_Damage < 0 then
        printfn "LONG RANGE SENSORS ARE INOPERABLE."
    else
        printfn $"LONG RANGE SCAN FOR QUADRANT {quadrantName state.CurrentQuadrant}"
        printfn "   "
        printfn "+-----+-----+-----+"

        for i = fst state.CurrentQuadrant - 1 to fst state.CurrentQuadrant + 1 do
            for j = snd state.CurrentQuadrant - 1  to snd state.CurrentQuadrant + 1 do
                match getQuadrant state (i, j) with
                | None -> printf "| *** "
                | Some q -> printf $"| {q.Klingons:D1}{q.Starbases:D1}{q.Stars:D1} "
            printfn "|"
            printfn "+-----+-----+-----+"

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

let validNav() =
    printfn "NAVIGATION DIRECTIONS"
    printfn "====================="
    printfn "2   1   8"
    printfn "  \ ' /"
    printfn "3 - * - 7"
    printfn "  / ' \ "
    printfn "4   5   6"
    

let computer(state : State) =
    let commandMenu() =
        printfn "   "
        printfn "FUNCTIONS AVAILABLE FROM LIBRARY-COMPUTER::"
        printfn "0 = CUMULATIVE GALACTIC RECORD"
        printfn "1 = STATUS REPORT"
        printfn "2 = PHOTON TORPEDO DATA"
        printfn "3 = STARBASE NAV DATA"
        printfn "4 = DIRECTION/DISTANCE CALCULATOR"
        printfn "5 = GALAXY 'REGION NAME' MAP"
        printfn "6 = GALAXY MAP"
        printfn "7 = NAVIGATION DIRECTIONS"
        printfn "8 = EXIT LIBRARY-COMPUTER"
        printfn "   "
        inputString "COMMAND ? "

    let getCommand() =
        let mutable validCommand = false;
        let mutable cmdOption = ""

        while not validCommand do
            let mutable cmd = commandMenu()
            cmdOption <- match cmd with
                            | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" -> validCommand <- true; cmd;
                            | _ -> printfn "Invalid command"; "";
            printfn ""

        cmdOption

    let mutable isOk = true
    while isOk do
        match getCommand() with
                | "1" -> computerStatusReport state
                | "2" -> computerPhotonTorpedoData state
                | "6" -> galaxyMap(state.CurrentQuadrant)
                | "7" -> validNav() 
                | "8" -> isOk <- false
                | _ -> ();
    state

let endOfMission(state) =
    printfn $"THERE WERE {state.TotalKlingons} KLINGON BATTLE CRUISERS LEFT AT"
    printfn "THE END OF YOUR MISSION."
    printfn ""
    printfn ""
    if state.TotalStarbases <> 0 then
        printfn "THE FEDERATION IS IN NEED OF A NEW STARSHIP COMMANDER"
        printfn "FOR A SIMILAR MISSION -- IF THERE IS A VOLUNTEER,"
        printf "LET HIM STEP FORWARD AND ENTER 'AYE' : "
        if readLine() = "AYE" then
            { state with StartAgain = true }            
        else
            state
    else
        state

let mainLoop() =
    start() |> ignore

    let commands = [
            ("NAV", "TO SET COURSE"); 
            ("SRS", "FOR SHORT RANGE SENSOR SCAN"); 
            ("LRS", "FOR LONG RANGE SENSOR SCAN"); 
            ("PHA", "TO FIRE PHASERS"); 
            ("TOR", "TO FIRE PHOTON TORPEDOES"); 
            ("SHE", "FOR SHIELD CONTROL"); 
            ("DAM", "TO GET DAMAGE REPORTS"); 
            ("COM", "TO CALL ON LIBRARY-COMPUTER"); 
            ("XXX", "TO RESIGN YOUR COMMAND")
        ]

    let commandMenu() =
        printfn "   "
        printfn "ENTER ONE OF THE FOLLOWING COMMANDS:"
        commands |> List.iter(fun (cmd, desc) -> printfn $"{cmd} - {desc}")
        printfn "   "
        inputString "COMMAND ? "

    let mutable state = startGame createState
    shortRangeScan state |> ignore
    let mutable isOk = true
    
    while isOk do
        state <- match commandMenu() with
                 | "NAV" -> navigate state
                 | "SRS" -> shortRangeScan state
                 | "LRS" -> longRangeScan state
                 | "PHA" -> pha state
                 | "TOR" -> tor state
                 | "SHE" -> she state
                 | "DAM" -> dam state
                 | "COM" -> computer state
                 | "XXX" -> isOk <- false; endOfMission(state)
                 | _ -> printfn "Invalid command"; state

    state.StartAgain

[<EntryPoint>]
let main argv =
    let mutable isOk = true

    while isOk do
        isOk <- mainLoop()

    0
