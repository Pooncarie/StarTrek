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
open Domain
open QuadrantNames
open Input
open Computer

let quadrantCreate x y =

    let klingonCount =
        match rnd.NextDouble() with
            | x when x > 0.98 -> 3
            | x when x > 0.95 -> 2
            | x when x > 0.80 -> 1
            | _ -> 0

    let starBaseCount =
        match rnd.NextDouble() with
            | x when x > 0.96 -> 1
            | _ -> 0

    let starCount =
        int  (rnd.NextDouble() * 7.98 + 1.01)

    let quadrant = { 
        Starbases = starBaseCount
        Stars = starCount
        Klingons = klingonCount
        QuadrantId = (x, y)
        Sectors = Array2D.init maxSectors maxSectors (fun i j -> createEmptySpace (i, j))
    }

    quadrant

let createState = 
    let createGalaxy = { Quadrants = Array2D.init maxQuadrants maxQuadrants (fun i j -> quadrantCreate i j) }

    let arrayOfMove = [|
        [| 0; 1|]
        [| -1; 1|]
        [| -1; 0|]
        [| -1; -1|]
        [| 0; -1|]
        [| 1; -1|]
        [| 1; 0|]
        [| 1; 1|]
        [| 0; 1|]
        |]

    let tmpStarDate = initialStardate

    let s = {
        Galaxy = createGalaxy
        Enterprise = createEnterprise (0,0) 
        StarDate = tmpStarDate
        StartedOnStardate = tmpStarDate
        NumberOfStarDays = 25 + (int) (rnd.NextDouble() * 10.0)
        CurrentQuadrant = QuadrantId(0, 0)
        CurrentSector = SectorId(0,0)
        TotalKlingons = 0
        TotalStarbases = 0
        TotalStars = 0
        DirectionArray = Array2D.init 9 2 (fun i j -> arrayOfMove[i][j])
        StartAgain = false;
        Error = false;
        }

    let mutable totalStarbases = 0
    let mutable totalKlingons = 0
    let mutable totalStars = 0


    for i in quadrantRange do
        for j in quadrantRange do
            let quadrant = s.Galaxy.Quadrants[i, j]
            totalStarbases <- totalStarbases + quadrant.Starbases
            totalKlingons <- totalKlingons + quadrant.Klingons
            totalStars <- totalStars + quadrant.Stars

    { s with TotalKlingons = totalKlingons; TotalStarbases = totalStarbases; TotalStars = totalStars;  }

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

    let printIt (s : Sector)  =
        match s with
        | Klingon k -> printf $" {k.Symbol}"
        | Enterprise e -> printf $" {e.Symbol}"
        | Star s -> printf $" {s.Symbol}"
        | Starbase s -> printf $" {s.Symbol}"
        | EmptySpace e -> printf $" {e.Symbol}"

    let sector = (currentQuadrant state).Sectors;

    for i in sectorRange do 
       printIt(sector[line, i])
    done

let shortRangeScan state = 
    let printCondition enterprise =
        let cc = Console.ForegroundColor
        match enterprise.Condition with
        | "GREEN" -> Console.ForegroundColor <- ConsoleColor.Green; 
        | "YELLOW" -> Console.ForegroundColor <- ConsoleColor.Yellow;
        | "*RED*" -> Console.ForegroundColor <- ConsoleColor.Red;
        | "DOCKED" -> Console.ForegroundColor <- ConsoleColor.Blue;
        | _ -> Console.ForegroundColor <- ConsoleColor.White;
        printf $"{enterprise.Condition}"
        Console.ForegroundColor <- cc

    printfn ""
    printfn $"SHORT RANGE SCAN FOR QUADRANT {(quadrantName state.CurrentQuadrant)}"
    printfn "   "
    printfn " +--1---2---3---4---5---6---7---8-+"
    printf "1|"
    printSector state 0; printf "|1";
    printfn $"        STARDATE            {state.StarDate}"
    printf "2|"
    printSector state 1; printf "|2";
    printf $"        CONDITION           "; printCondition state.Enterprise; printfn ""
    printf "3|"
    printSector state 2; printf "|3";
    printfn $"        QUADRANT            {fst state.CurrentQuadrant + 1},{snd state.CurrentQuadrant + 1}"
    printf "4|"
    printSector state 3; printf "|4";
    printfn $"        SECTOR              {fst state.CurrentSector + 1},{snd state.CurrentSector + 1}"
    printf "5|"
    printSector state 4; printf "|5";
    printfn $"        PHOTO TORPEDOES     {state.Enterprise.Torpedoes}"
    printf "6|"
    printSector state 5; printf "|6";
    printfn $"        TOTAL ENERGY        {state.Enterprise.Energy + state.Enterprise.ShieldEnergy}"
    printf "7|"
    printSector state 6; printf "|7";
    printfn $"        SHIELDS             {state.Enterprise.ShieldEnergy}"
    printf "8|"
    printSector state 7; printf "|8";
    printfn $"        KLINGONS REMAINING  {state.TotalKlingons}"
    printf " +--------------------------------+"
    printfn "   "
    state

let changeQuadrant state newQuadrant =

    let clearSectors quadrant = 
        for i in quadrantRange do
            for j in quadrantRange do
                match quadrant.Sectors[i, j] with
                | EmptySpace _ -> ()
                | _ -> quadrant.Sectors[i, j] <- createEmptySpace (i,j)

    let randomSectorIndexs = Seq.initInfinite (fun _ -> (rnd.Next(0, maxSectors - 1), rnd.Next(0, maxSectors - 1))) |> Seq.distinct |> Seq.take(40) |> Seq.toArray
    let quadrant = state.Galaxy.Quadrants[fst newQuadrant, snd newQuadrant]

    clearSectors quadrant
    let mutable cnt = 0

    printfn ""
    printfn $"NOW ENTERING {quadrantName newQuadrant} QUADRANT . . ."

    if quadrant.Klingons > 0 then
        printfn "COMBAT AREA **** CONDITION RED"
        if (state.Enterprise.ShieldEnergy < 200) then
            printfn "SHIELDS DANGEROUSLY LOW"
        quadrant.Sectors[fst randomSectorIndexs[0], snd randomSectorIndexs[0]] 
            <- copyEnterprise { state.Enterprise with Condition = "*RED*"; SectorId =  randomSectorIndexs[cnt] }
    else
        quadrant.Sectors[fst randomSectorIndexs[0], snd randomSectorIndexs[0]] 
            <- copyEnterprise { state.Enterprise with SectorId =  randomSectorIndexs[cnt]; Condition = "GREEN" }

    cnt <- cnt + 1
    
    // TODO: THIS IS JUST FOR TESTING
   // quadrant.Sectors.[fst sectorIndexs[cnt], snd sectorIndexs[cnt]]
    //    <- createStarbase sectorIndexs[cnt]
    //cnt <- cnt + 1

    [0..quadrant.Klingons - 1] |> List.iter(fun x -> 
        let newKlingonSector = randomSectorIndexs[x + cnt]
        quadrant.Sectors[fst newKlingonSector, snd newKlingonSector] 
            <- createKlingon randomSectorIndexs[cnt]
        cnt <- cnt + 1
    )

    [0..quadrant.Starbases - 1] |> List.iter(fun x -> 
        let newStarbaseSector = randomSectorIndexs[x + cnt]
        quadrant.Sectors[fst newStarbaseSector, snd newStarbaseSector] 
            <- createStarbase randomSectorIndexs[cnt] 
        cnt <- cnt + 1
    )

    [0..quadrant.Stars - 1] |> List.iter(fun x -> 
        let newStarSector = randomSectorIndexs[x + cnt]
        quadrant.Sectors[fst newStarSector, snd newStarSector] 
            <- createStar randomSectorIndexs[cnt] 
        cnt <- cnt + 1
    )

    { state with 
        CurrentSector = randomSectorIndexs[0]; 
        CurrentQuadrant = newQuadrant;  
        Enterprise = match quadrant.Sectors[fst randomSectorIndexs[0], snd randomSectorIndexs[0]] with | Enterprise e -> e | _ -> state.Enterprise
        }

let startGame state =
    let newQuadrant = (rnd.Next(0, maxQuadrants - 1), rnd.Next(0, maxQuadrants - 1))
    let newState = changeQuadrant state newQuadrant

    printfn "YOUR ORDERS ARE AS FOLLOWS:"
    printfn $"   DESTROY THE {state.TotalKlingons} KLINGON WARSHIPS WHICH HAVE INVADED" 
    printfn "   THE GALAXY BEFORE THEY CAN ATTACK FEDERATION HEADQUARTERS"
    printfn $"   ON STARDATE {state.StarDate + state.NumberOfStarDays}. THIS GIVES YOU {(state.NumberOfStarDays)} DAYS. THERE ARE {state.TotalStarbases}"   
    printfn "   STARBASES IN THE GALAXY FOR RESUPPLYING YOUR SHIP."
    printfn ""
    printfn ""
    printfn "YOUR MISSION BEGINS WITH YOUR STARSHIP LOCATED"
    printfn $"IN THE GALACTIC QUADRANT {(quadrantName newState.CurrentQuadrant)}"
    newState

let getCourse() : double option =
    let courseError() =
        printfn ""
        printfn "   LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"
        printfn ""

    let mutable course = (inputDouble "COURSE (1-9) ")
    if course >= 8.5 then
        course <- 1.0
    
    if course < 1 || course > 8.999 then
        courseError()
        None
    else
        Some (course - 1.0)

let getWarp (state : State) : (int * double) option =
    let warpError warpSpeed =
        printfn "   CHIEF ENGINEER SCOTT REPORTS, 'THE ENGINES WON'T TAKE"
        printfn $"   WARP {warpSpeed}!'"; 
        printfn "";

    let warpRange = 
        if state.Enterprise.WarpEngines < 0 then
            "WARP FACTOR (0-0.2) "
        else
            "WARP FACTOR (0-8) "

    let warpFactor = inputDouble warpRange

    if state.Enterprise.WarpEngines < 0 && warpFactor > 0.2 then
        printfn "WARP ENGINES ARE DAMAGED.  MAXIUM SPEED = WARP 0.2"
        None
    else
        if warpFactor > 0 && warpFactor <= 8 then
            let n = int (warpFactor * 8.0 + 0.5)
            if state.Enterprise.Energy - n >= 0 then
                Some (n, warpFactor)
            else
                printfn "ENGINEERING REPORTS 'INSUFFICIENT ENERGY AVAILABLE FOR MANEUVERING AT WARP {warpFactor}!'"
               
                if state.Enterprise.ShieldEnergy < n - state.Enterprise.Energy || state.Enterprise.ShieldControl < 0 then
                    None
                else
                    printfn $"DEFLECTOR CONTROL ROOM ACKNOWLEDGES {state.Enterprise.ShieldEnergy} UNITS OF ENERGY PRESENTLY DEPLOYED TO SHIELDS."
                    None
        else 
            if warpFactor = 0.0 then
                None
            else
                warpError warpFactor
                None

let navigateQuadrant state x y : State =
    if fst state.CurrentQuadrant + x < 0 || fst state.CurrentQuadrant + x > maxQuadrants - 1 || snd state.CurrentQuadrant + y < 0 || snd state.CurrentQuadrant + y > maxQuadrants - 1 then
        printfn "LT. UHURA REPORTS MESSAGE FROM STARFLEET COMMAND:"
        printfn "  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER"
        printfn "  IS HEREBY *DENIED*.  SHUT DOWN YOUR ENGINES.'"
        printfn "CHIEF ENGINEER SCOTT REPORTS  'WARP ENGINES SHUT DOWN"
        printfn $"AT SECTOR {fst state.CurrentSector + 1},{snd state.CurrentSector + 1} OF QUADRANT {fst state.CurrentQuadrant + 1},{snd state.CurrentQuadrant + 1}"
        { state with Error = true;}
    else
        let newQuadrant = (fst state.CurrentQuadrant + x, snd state.CurrentQuadrant + y)
        
        changeQuadrant state newQuadrant

let getDecimalPart (num : double) = num - Math.Truncate(num)

let getDevice enterprise i =
    match i with
    | 1 -> enterprise.WarpEngines
    | 2 -> enterprise.ShortRangeSensors
    | 3 -> enterprise.LongRangeSensors
    | 4 -> enterprise.PhaserControl
    | 5 -> enterprise.PhotonTubes
    | 6 -> enterprise.DamageControl
    | 7 -> enterprise.ShieldControl
    | 8 -> enterprise.LibraryComputer
    | _ -> 0

let setDevice enterprise i value =
    match i with
    | 1 -> { enterprise with WarpEngines = enterprise.WarpEngines + value }
    | 2 -> { enterprise with ShortRangeSensors = enterprise.ShortRangeSensors + value }
    | 3 -> { enterprise with LongRangeSensors = enterprise.LongRangeSensors + value }
    | 4 -> { enterprise with ShieldControl = enterprise.ShieldControl + value }
    | 5 -> { enterprise with LibraryComputer = enterprise.LibraryComputer + value }
    | 6 -> { enterprise with PhaserControl = enterprise.PhaserControl + value }
    | 7 -> { enterprise with PhotonTubes = enterprise.PhotonTubes + value }
    | 8 -> { enterprise with DamageControl = enterprise.DamageControl + value }
    | _ -> enterprise

let getDeviceName i =
    match i with
    | 1 -> "WARP ENGINES"
    | 2 -> "SHORT RANGE SENSORS"
    | 3 -> "LONG RANGE SENSORS"
    | 4 -> "PHASER CONTROL"
    | 5 -> "PHOTON TUBES"
    | 6 -> "DAMAGE CONTROL"
    | 7 -> "SHIELD CONTROL"
    | 8 -> "LIBRARY COMPUTER"
    | _ -> $"**** UNKOWN DEVICE{i} *****"

let getCondition state =
    let quadrant = currentQuadrant state
    if state.Enterprise.Condition = "DOCKED" then
        "DOCKED"
    else if state.Enterprise.Energy < initialEnergy / 10 then
        "YELLOW"
    else if quadrant.Klingons > 0 then
        "*RED*"
    else
        "GREEN"

let endOfGame state =
    printfn ""
    printfn "THE ENTERPRISE HAS BEEN DESTROYED.  THEN FEDERATION ";
    printfn "WILL BE CONQUERED"
    printfn $"IT IS STARDATE {state.StarDate + state.NumberOfStarDays}"
    Environment.Exit(0)

(* LINE 6000 *)
let klingonsShooting state =
    let quadrant = currentQuadrant state 
    let mutable shieldUnits = state.Enterprise.ShieldEnergy
    let mutable enterprise = state.Enterprise

    if quadrant.Klingons > 0 then
        if state.Enterprise.IsDocked then
             printf "STARBASE SHIELDS PROTECT THE ENTERPRISE"
        else
            for i in sectorRange do
                for j in sectorRange do
                    match quadrant.Sectors[i, j] with
                    | Klingon k -> 
                        let distance = sqrt (
                                square (double (fst k.SectorId - fst state.CurrentSector)) +
                                square (double (snd k.SectorId - snd state.CurrentSector))
                                )
                        let h = double k.ShieldStrength / distance * (rnd.NextDouble() + 2.0)

                        shieldUnits <- shieldUnits - int h
                        quadrant.Sectors[i, j] <- copyKlingon { k with ShieldStrength = int (double k.ShieldStrength / 3.0 + rnd.NextDouble()) }
                        printfn $"{h:N2} UNIT HIT ON ENTERPRISE FROM SECTOR {fst k.SectorId + 1},{snd k.SectorId + 1}"
                        if shieldUnits > 0 then
                            printfn $"   <SHIELDS DOWN TO {shieldUnits} UNITS>"
                            if h > 20 then
                                if rnd.NextDouble() > 0.6 || h / double shieldUnits <= 0.02 then
                                    ()
                                else
                                    let r1 = fnr
                                    let v = int (h / double shieldUnits - 0.5 * rnd.NextDouble())
                                    enterprise <- setDevice state.Enterprise r1 -v
                                    printfn  $"DAMAGE CONTROL REPORTS '{getDeviceName r1} DAMAGED BY THE HIT'"
                        else
                            endOfGame state

                    | _ -> ()
        
    { state with Enterprise = { enterprise with ShieldEnergy = shieldUnits; Condition = getCondition state } }

let navigateSector state (course : double) : State =
    let icourse = int (Math.Round course)

    let x1 = int (double state.DirectionArray[icourse,0] + double (state.DirectionArray[icourse + 1,0] - state.DirectionArray[icourse, 0]) * getDecimalPart course)
    let x2 = int (double state.DirectionArray[icourse,1] + double (state.DirectionArray[icourse + 1,1] - state.DirectionArray[icourse, 1]) * getDecimalPart course)

    let dockedWithStarbase state quadrant sector =
        (* we have docked with starbase, update Energy, Torpedoes & ShieldEnergy to reflect this *)
        printfn " SHIELDS DROPPED FOR DOCKING PURPOSES"
           
        let newEnterprise = copyEnterprise { state.Enterprise with 
                                                Energy = initialEnergy; 
                                                ShieldEnergy = initialShieldStrength; 
                                                Torpedoes = initialTorpedoes; 
                                                Condition = "DOCKED";
                                                SectorId = (fst state.CurrentSector + x1,snd state.CurrentSector + x2)
                                                IsDocked = true
                                            }
        for i in sectorRange do
            for j in sectorRange do
                match quadrant.Sectors.[i, j] with
                | Enterprise _ -> 
                    if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                        quadrant.Sectors[i, j] <- sector
                    quadrant.Sectors.[i, j] <- createEmptySpace (i, j)
                | _ -> ()

                if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector +  x2 then
                    quadrant.Sectors[i, j] <- newEnterprise

        { state with CurrentSector = (fst state.CurrentSector + x1, snd state.CurrentSector + x2); Enterprise = match newEnterprise with | Enterprise e -> e |  _ -> state.Enterprise }

    let moveEnterprise state quadrant =
        let newState = if state.Enterprise.IsDocked then
                        if quadrant.Klingons > 0 then
                            { state with Enterprise = { state.Enterprise with IsDocked = false; Condition = "*RED*" } }
                        else
                            { state with Enterprise = { state.Enterprise with IsDocked = false; Condition = "GREEN" } }
                       else
                            state

        for i in sectorRange do
            for j in sectorRange do
                match quadrant.Sectors[i, j] with
                | Enterprise _ -> 
                    quadrant.Sectors[i, j] <- createEmptySpace (i, j)
                | _ -> ()

                if i = fst newState.CurrentSector + int x1 && j = snd newState.CurrentSector + int x2 then
                    quadrant.Sectors[i, j] <- copyEnterprise { newState.Enterprise with SectorId = (i,j)}

        { newState with CurrentSector = (fst state.CurrentSector + int x1, snd state.CurrentSector + int x2); }

    if fst state.CurrentSector + x1 < 0 || fst state.CurrentSector + x1 >= maxSectors || snd state.CurrentSector + x2 < 0 || snd state.CurrentSector + x2 >= maxSectors then
        navigateQuadrant state x1 x2
    else
        let quadrant = state.Galaxy.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant]
        let sector = quadrant.Sectors.[fst state.CurrentSector + x1, snd state.CurrentSector + x2]

        match sector with
        | Klingon k ->
            printfn $"WARP ENGINES SHUT DOWN AT SECTOR {fst state.CurrentSector + 1},{snd state.CurrentSector + 1} DUE TO BAD NAVIGATION"
            { state with Error = true }
        | Star s ->
            printfn $"WARP ENGINES SHUT DOWN AT SECTOR {fst state.CurrentSector + 1},{snd state.CurrentSector + 1} DUE TO BAD NAVIGATION"
            { state with Error = true }
        | Starbase s ->  dockedWithStarbase state quadrant sector
        | _ -> moveEnterprise state quadrant

        
let navigate state =
    let course = getCourse()
    
    let klingonsMove state = state

    (* 2702 *)
    let klingonsFire state warpSpeed = 
        let newState = klingonsShooting state
        let mutable enterprise = newState.Enterprise
        let d6 = if warpSpeed >= 1 then 1 else warpSpeed
        for i in [1..8] do
            if getDevice enterprise i < 0 then
                enterprise <- setDevice enterprise i d6
                let v = getDevice enterprise i
                if v > -0.1 && v < 0 then 
                    enterprise <- setDevice enterprise i -0.1
                if getDevice enterprise i > 0 then
                    printfn $"DAMAGE CONTROL REPORTS {getDeviceName i} REPAIRED"
        
        if rnd.NextDouble() <= 0.2 then
            let r1 = fnr
            if rnd.NextDouble() < 0.6 then
                let v = getDevice enterprise r1
                enterprise <- setDevice enterprise r1 (v - (rnd.NextDouble() * 5.0 + 1.0))
                printfn $"DAMAGE CONTROL REPORT: {getDeviceName r1} DAMAGED"
            else
                let v = getDevice enterprise r1
                enterprise <- setDevice enterprise r1 (v + (rnd.NextDouble() * 3.0 + 1.0))
                printfn $"DAMAGE CONTROL REPORT: {getDeviceName r1} STATE OF REPAIR IMPROVED"
                printfn ""

        { newState with Enterprise = enterprise }

    match course with
    | None -> state
    | Some course ->
        let warp = getWarp state
        printfn ""

        match warp with
        | None -> state
        | Some warp ->
            let (warpSpeed, warpFactor) = warp
            let mutable s1 = klingonsFire state (int warpSpeed)

            let mutable t8 = 1
            if warpFactor < 1 then
                t8 <- int (10.0 * warpFactor) / 10
            s1 <- { s1 with StarDate = s1.StarDate + t8 }

            for i = 0 to (int warpSpeed) - 1 do
                if s1.Error = false then
                    s1 <- navigateSector s1 course
            shortRangeScan { s1 with Error = false;}

let longRangeScan state =
    let getQuadrant state qadrantId : Quadrant option =
        if (fst qadrantId) < 0 || (fst qadrantId) >= maxQuadrants || (snd qadrantId) < 0 || (snd qadrantId) >= maxQuadrants then
            None
        else
            Some state.Galaxy.Quadrants.[fst qadrantId, snd qadrantId]

    if state.Enterprise.LongRangeSensors < 0 then
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

let firePhasers state =
    let getPhaserUnits =
        let mutable ok = false
        let mutable noOfUnits = 0.0
        let energy = state.Enterprise.Energy

        while not ok do
            printfn $"ENERGY AVAILABLE = {energy} UNITS"

            noOfUnits <- inputDouble "NUMBER OF UNITS TO FIRE: "

            if noOfUnits > 0 && noOfUnits < energy then
                ok <- true

        double noOfUnits

    let mutable klingonsDestroyed = 0
    let mutable energyUsed = 0.0

    if state.Enterprise.PhaserControl < 0 then
        printfn "PHASER CONTROL IS INOPERABLE."
    else
        let klingons = getKlingons state

        if klingons.Length = 0 then 
            printfn ""
            printfn "SCIENCE OFFICER SPOCK REPORTS  'SENSORS SHOW NO ENEMY SHIPS IN THIS QUADRANT'"
        else 
            if state.Enterprise.LibraryComputer < 0 then
                printfn "COMPUTER FAILURE HAMPERS ACCURACY"

            printfn "PHASERS LOCKED ON TARGET;  "
          
               
            energyUsed <- getPhaserUnits

            if state.Enterprise.ShieldControl < 0 then
                energyUsed <-  energyUsed * rnd.NextDouble() / double klingons.Length

            let h1 = energyUsed / double klingons.Length
            let quadrant = currentQuadrant state 

            for i in sectorRange do
                for j in sectorRange do
                    match quadrant.Sectors[i, j] with
                    | Klingon k -> 
                        let distance = sqrt (
                                square (double (fst k.SectorId - fst state.CurrentSector)) +
                                square (double (snd k.SectorId - snd state.CurrentSector))
                                )
                        let h = (h1 / distance) * (rnd.NextDouble() + 2.0)
                    
                        if h <= 0.15 * double k.ShieldStrength then
                            printfn $"SENSORS SHOW NO DAMAGE TO ENEMY AT {fst k.SectorId + 1},{snd k.SectorId + 1}"
                        else
                            printfn $"{int h} UNIT HIT ON KLINGON AT SECTOR {fst k.SectorId + 1}, {snd k.SectorId + 1}"
                            if k.ShieldStrength - int h < 0 then
                                printfn "**** KLINGON DESTROYED ****"
                                quadrant.Sectors[i, j]  <- createEmptySpace (i, j)
                                state.Galaxy.Quadrants[fst state.CurrentQuadrant, snd state.CurrentQuadrant] <- { quadrant with Klingons = quadrant.Klingons - 1 }
                                klingonsDestroyed <- klingonsDestroyed + 1
                            else
                                printfn $"   (SENSORS SHOW {k.ShieldStrength} UNITS REMAINING)"
                                quadrant.Sectors[i, j]  <- copyKlingon { k with ShieldStrength = k.ShieldStrength - int h }
                    | _ -> ()


    let s1 =  { state with 
                    TotalKlingons = (state.TotalKlingons - klingonsDestroyed); 
                    Enterprise = { 
                        state.Enterprise with Energy = state.Enterprise.Energy - int energyUsed; Condition = getCondition state 
                    } 
               }

    shortRangeScan (klingonsShooting s1)
            
(* 4700 *)
let photonTorpedoes state =
    let getCourse() : double option =
        let courseError() =
            printfn ""
            printfn "ENSIGN CHEKOV REPORTS, 'INCORRECT COURSE DATA, SIR!'"
            printfn ""

        let mutable course = (inputDouble "PHOTON TORPEDO COURSE (1-8) ")
        if course >= 8.5 then
            course <- 1.0
    
        if course < 1 || course > 8.999 then
            courseError()
            None
        else
            Some (course - 1.0)

    let canUsePhotonTorpedoes state =
        if state.Enterprise.Torpedoes > 0 then
            if state.Enterprise.PhotonTubes >= 0 then
                let course = getCourse() 
                match course with
                | Some c -> (true, c)
                | None -> (false, 0)
            else
                printfn "PHOTON TUBES ARE NOT OPERATIONAL"
                (false, 0)
        else
            printfn "ALL PHOTON TORPEDOES EXPENDED"        
            (false, 0)

    let (canFire, course) = canUsePhotonTorpedoes state
    let mutable klingonsDestroyed = 0
    let mutable starbasesDestroyed = 0
    let mutable newState = state

    if canFire then
        let icourse = int (Math.Round course)

        let x1 = (double state.DirectionArray[icourse,0] + double (state.DirectionArray[icourse + 1,0] - state.DirectionArray[icourse, 0]) * getDecimalPart course)
        let x2 = (double state.DirectionArray[icourse,1] + double (state.DirectionArray[icourse + 1,1] - state.DirectionArray[icourse, 1]) * getDecimalPart course)

        let mutable x = double (fst state.CurrentSector)
        let mutable y = double (snd state.CurrentSector)

        printfn "TORPEDO TRACK:"
        let mutable flag = true

        while flag do
            x <- x + x1
            y <- y + x2
            let x3 = int (x + 0.5)
            let y3 = int (y + 0.5)
        
            if x3 < 0 || x3 > maxSectors - 1 || y3 < 0 || y3 > maxSectors - 1 then
                printfn "TORPEDO MISSED"
                flag <- false
            else
               printfn $"               {x3 + 1},{y3 + 1}"
               let quadrant = currentQuadrant newState
               let sector = quadrant.Sectors[x3, y3] 
               flag <- match sector with
                        | EmptySpace e -> true
                        | Klingon k -> 
                            printfn "*** KLINGON DESTROYED ***"
                            quadrant.Sectors[x3, y3]  <- createEmptySpace (x3, y3)
                            newState.Galaxy.Quadrants[fst state.CurrentQuadrant, snd state.CurrentQuadrant] <- { quadrant with Klingons = quadrant.Klingons - 1 }
                            klingonsDestroyed <- klingonsDestroyed + 1
                            false
                        | Star s -> 
                            printfn $"STAR AT {x3+1}, {y3+1} ABSORBED TORPEDO ENERGY."
                            newState <- klingonsShooting state
                            false
                        | Starbase s -> 
                            printfn $"*** STARBASE DESTROYED ***"
                            starbasesDestroyed <- starbasesDestroyed + 1
                            quadrant.Sectors[x3, y3]  <- createEmptySpace (x3, y3)
                            newState.Galaxy.Quadrants[fst state.CurrentQuadrant, snd state.CurrentQuadrant] <- { quadrant with Starbases = quadrant.Starbases - 1 }
                            if newState.TotalStarbases = 1 then
                                printfn " THAT DOES IT, CAPTAIN!!  YOU ARE HEREBY RELIEVED OF COMMAND"
                                printfn "AND SENTENCED TO 99 STARDATES AT HARD LABOR ON CYGNUS 12!!"
                            else
                                printfn $"STARFLEET COMMAND REVIEWING YOUR RECORD TO CONSIDER"
                                printfn "COURT MARTIAL!!"
                            false
                        | _ -> false

    let s1 =  { newState with 
                    TotalKlingons = (state.TotalKlingons - klingonsDestroyed); 
                    TotalStarbases = (state.TotalStarbases - starbasesDestroyed);
                    Enterprise = { 
                        state.Enterprise with Energy = state.Enterprise.Energy - 2; 
                                                Condition = getCondition state; 
                                                Torpedoes = state.Enterprise.Torpedoes - 1
                    } 
              }

    shortRangeScan (klingonsShooting s1)

(* 5530 *)
let shieldControl state =
    let doShieldControl state = 
        if state.Enterprise.ShieldControl >= 0 then
            printfn $"ENERGY AVAILABLE = {state.Enterprise.Energy + state.Enterprise.ShieldEnergy}"
            let shieldEnergy = inputInteger "NUMBER OF UNITS TO SHIELDS: "
            match shieldEnergy with
            | shieldEnergy when shieldEnergy > state.Enterprise.Energy + state.Enterprise.ShieldEnergy -> 
                printfn "SHIELD CONTROL REPORTS  'THIS IS NOT THE FEDERATION TREASURY.'"
                printfn "<SHIELDS UNCHANGED>"
                state
            | shieldEnergy when shieldEnergy < 0 || shieldEnergy = state.Enterprise.ShieldEnergy ->
                printfn "<SHIELDS UNCHANGED>"
                state
            | shieldEnergy when shieldEnergy > state.Enterprise.Energy + state.Enterprise.ShieldEnergy ->
                printfn "SHIELD CONTROL REPORTS  'THIS IS NOT THE FEDERATION TREASURY.'"
                printfn "<SHIELDS UNCHANGED>"
                state
            | _ ->
                printfn $"DEFLECTOR CONTROL ROOM REPORTS SHIELDS NOW AT {shieldEnergy} UNITS"
                let wrk = state.Enterprise.Energy + state.Enterprise.ShieldEnergy - shieldEnergy
                { state with Enterprise = { state.Enterprise with ShieldEnergy = shieldEnergy; Energy = wrk } }
        else
            printfn "SHIELD CONTROL INOPERABLE"
            state

    printfn ""
    doShieldControl state |> shortRangeScan

(* 5690 *)
let damageControl state =
    printfn ""
    if state.Enterprise.DamageControl >= 0 then
        printfn "DEVICE STATE OF REPAIR:"
        printfn $"   WARP ENGINES = {state.Enterprise.WarpEngines:N2}"
        printfn $"   SHORT RANGE SENSORS = {state.Enterprise.ShortRangeSensors:N2}"
        printfn $"   LONG RANGE SENSORS = {state.Enterprise.LongRangeSensors:N2}"
        printfn $"   PHASER CONTROL = {state.Enterprise.PhaserControl:N2}"
        printfn $"   PHOTON TUBES = {state.Enterprise.PhotonTubes:N2}"
        printfn $"   DAMAGE CONTROL = {state.Enterprise.DamageControl:N2}"
        printfn $"   SHIELD CONTROL = {state.Enterprise.ShieldControl:N2}"
        printfn $"   LIBRARY COMPUTER = {state.Enterprise.LibraryComputer:N2}"
    else
        printfn "DAMAGE CONTROL REPORT NOT AVAILABLE"
    state

let endOfMission state =
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

let missionCompleted state =
    if state.StartAgain then
        true
    else
        if state.TotalKlingons = 0 then
            printfn "CONGRATULATIONS, CAPTAIN! THE LAST KLINGON BATTLE CRUISER"
            printfn "MENACING THE FEDERATION HAS BEEN DESTROYED."
            printfn ""
            printfn "YOUR EFFICIENCY RATING IS {state.NumberOfStarDays - state.StarDate}."
            false
        else
            true

let isEndOfGame state =
    if state.Enterprise.Energy <= 0 then
        endOfGame state
        false
    else
        true

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
    state <- shortRangeScan state
    let mutable isOk = true
    
    while isOk do
        state <- match commandMenu() with
                 | "NAV" -> navigate state
                 | "SRS" -> shortRangeScan state
                 | "LRS" -> longRangeScan state
                 | "PHA" -> firePhasers state
                 | "TOR" -> photonTorpedoes state
                 | "SHE" -> shieldControl state
                 | "DAM" -> damageControl state
                 | "COM" -> computer state
                 | "XXX" -> isOk <- false; endOfMission(state)
                 | _ -> printfn "Invalid command"; state

        if isOk then isOk <- missionCompleted state

    state.StartAgain

[<EntryPoint>]
let main argv =
    let mutable isOk = true

    while isOk do
        isOk <- mainLoop()

    0
