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

let square x = x * x

let currentQuadrant (state : State) = state.Galaxy.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant]


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

    let quadrant = { 
        Starbases = starBaseCount()
        Stars = starCount()
        Klingons = klingonCount()
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

    let s = {
        Galaxy = createGalaxy
        Enterprise = createEnterprise (0,0) 
        StarDate = initialStardate
        NumberOfStarDays = 25 + (int) (rnd.NextDouble() * 10.0)
        CurrentQuadrant = QuadrantId(0, 0)
        CurrentSector = SectorId(0,0)
        TotalKlingons = 0
        TotalStarbases = 0
        TotalStars = 0
        DirectionArray = Array2D.init 8 2 (fun i j -> arrayOfMove[i][j])
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

    let sector = state.Galaxy.Quadrants[fst state.CurrentQuadrant, snd state.CurrentQuadrant].Sectors;

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
        | _ -> Console.ForegroundColor <- ConsoleColor.White;
        printf $"{enterprise.Condition}"
        Console.ForegroundColor <- cc

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
    printfn $"        TOTAL ENERGY        {state.Enterprise.Energy + state.Enterprise.ShieldStrength}"
    printf "7|"
    printSector state 6; printf "|7";
    printfn $"        SHIELDS             {state.Enterprise.ShieldStrength}"
    printf "8|"
    printSector state 7; printf "|8";
    printfn $"        KLINGONS REMAINING  {state.TotalKlingons}"
    printf " +--------------------------------+"
    printfn "   "
    state

let changeQuadrant state newQuadrant : State =

    let clearSectors quadrant = 
        for i in quadrantRange do
            for j in quadrantRange do
                match quadrant.Sectors.[i, j] with
                | EmptySpace _ -> ()
                | _ -> quadrant.Sectors[i, j] <- createEmptySpace (i,j)

    let sectorIndexs = Seq.initInfinite (fun _ -> (rnd.Next(0, maxSectors - 1), rnd.Next(0, maxSectors - 1))) |> Seq.distinct |> Seq.take(40) |> Seq.toArray
    let quadrant = state.Galaxy.Quadrants.[fst newQuadrant, snd newQuadrant]

    clearSectors quadrant
    let mutable cnt = 0

    printfn ""
    printfn $"NOW ENTERING {quadrantName newQuadrant} QUADRANT . . ."

    (* 
        TODO: Currently putting Enterprize into a new sector BUT I think it should be in the sector the user 
        has navigated to. This will is ok for the moment.
    *)
    if quadrant.Klingons > 0 then
        printfn "COMBAT AREA **** CONDITION RED"
        if (state.Enterprise.ShieldStrength < 200) then
            printfn "SHIELDS DANGEROUSLY LOW"
        quadrant.Sectors[fst sectorIndexs[0], snd sectorIndexs[0]] <- copyEnterprise { state.Enterprise with Condition = "*RED*"; SectorId =  sectorIndexs[cnt] }
    else
        quadrant.Sectors[fst sectorIndexs[0], snd sectorIndexs[0]] <- copyEnterprise { state.Enterprise with SectorId =  sectorIndexs[cnt]; Condition = "GREEN" }

    cnt <- cnt + 1

    [0..quadrant.Klingons - 1] |> List.iter(fun x -> 
        let newKlingonSector = sectorIndexs[x + cnt]
        quadrant.Sectors[fst newKlingonSector, snd newKlingonSector] 
            <- createKlingon  (fst sectorIndexs[cnt], snd sectorIndexs[cnt])
        cnt <- cnt + 1
    )

    [0..quadrant.Starbases - 1] |> List.iter(fun x -> 
        let newStarbaseSector = sectorIndexs[x + cnt]
        quadrant.Sectors[fst newStarbaseSector, snd newStarbaseSector] 
            <- createStarbase (fst sectorIndexs[cnt], snd sectorIndexs[cnt])
        cnt <- cnt + 1
    )

    [0..quadrant.Stars - 1] |> List.iter(fun x -> 
        let newStarSector = sectorIndexs[x + cnt]
        quadrant.Sectors[fst newStarSector, snd newStarSector] 
            <- createStar (fst sectorIndexs[cnt], snd sectorIndexs[cnt])
        cnt <- cnt + 1
    )

    { state with 
        CurrentSector = sectorIndexs[0]; 
        CurrentQuadrant = newQuadrant;  
        Enterprise = match quadrant.Sectors[fst sectorIndexs[0], snd sectorIndexs[0]] with | Enterprise e -> e | _ -> state.Enterprise
        }

let startGame (state : State) =
    let newQuadrant = (rnd.Next(0, 8), rnd.Next(0, 8))
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


(* Show all the quadrant names, mostly for debugging purposes. *)
let galaxyMap state  = 
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

let computerStatusReport state =
    printfn "STATUS REPORT:"
    printfn $"    KLINGONS LEFT: {state.TotalKlingons}"
    printfn $"    MISSION MUST BE COMPLETED IN {state.NumberOfStarDays} STARDATES"
    if state.TotalStarbases > 0 then
        printfn $"    THE FEDERATION IS MAINTAINING {state.TotalStarbases} STARBASES IN THE GALAXY"
    else
        printfn "    YOUR STUPIDITY HAS LEFT YOU ON YOUR ON IN THE GALAXY -- YOU HAVE NO STARBASES LEFT!"
    state

let distanceCalculator coords = 
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
    printfn $"DISTANCE = {sqrt (double (square y + square x)) }"
    ()
        
(* LINE 8150 *)   
let directionDistanceCalculator state =
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
    state

let getKlingons state =
    let mutable klingons = []
    let quadrant = currentQuadrant state

    for i in sectorRange do
        for j in sectorRange do
            match quadrant.Sectors[i, j] with
            | Klingon k -> klingons <- k :: klingons
            | _ -> ()
    klingons

let getStarbases state = 
    let mutable starbases = []
    let quadrant = currentQuadrant state

    for i in sectorRange do
        for j in sectorRange do
            match quadrant.Sectors.[i, j] with
            | Starbase s -> starbases <- s :: starbases
            | _ -> ()
    starbases

let computerPhotonTorpedoData(state : State) =
    getKlingons state |> List.iter(fun klingon -> 
        printf "FROM ENTERPRISE TO KLINGON BATTLE CRUISER "; 
        distanceCalculator { 
            InitialX = fst state.CurrentSector + 1;
            InitialY = snd state.CurrentSector + 1; 
            FinalX = fst klingon.SectorId + 1; 
            FinalY = snd klingon.SectorId + 1})

    state
    
let computerStarbaseData state =
    let starbases = getStarbases state

    if starbases.Length = 0 then
        printfn "MR. SPOCK REPORTS, 'SENSORS SHOW NO STARBASES IN THIS QUADRANT.'"
    else
        starbases |> List.iter(fun starbase -> 
            printf "FROM ENTERPRISE TO STARBASE " 
            distanceCalculator { 
                InitialX = fst state.CurrentSector + 1;
                InitialY = snd state.CurrentSector + 1; 
                FinalX = fst starbase.SectorId + 1; 
                FinalY = snd starbase.SectorId + 1})

    state

let getCourse() : int option =
    let courseError() =
        printfn ""
        printfn "   LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"
        printfn ""

    let course = (inputInteger "COURSE (0-8) ") - 1

    if course < 0 || course > 7 then
        courseError()
        None
    else
        Some course 

let getWarp (state : State) : int option =
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
                Some n
            else
                printfn "ENGINEERING REPORTS   'INSUFFICIENT ENERGY AVAILABLE"
                printfn $"                     FOR MANEUVERING AT WARP {warpFactor}!'"
               
                if state.Enterprise.ShieldStrength < n - state.Enterprise.Energy || state.Enterprise.ShieldControl < 0 then
                    None
                else
                    printfn $"DEFLECTOR CONTROL ROOM ACKNOWLEDGES  {state.Enterprise.ShieldStrength} UNITS OF ENERGY"
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

let navigateSector state course  : State =
    let x1 = state.DirectionArray[course,0]
    let x2 = state.DirectionArray[course,1]

    let dockedWithStarbase state quadrant sector =
        (* we have docked with starbase, update Energy, Torpedoes & ShieldEnergy to reflect this *)
        printfn " SHIELDS DROPPED FOR DOCKING PURPOSES"
           
        for i in sectorRange do
            for j in sectorRange do
                match quadrant.Sectors.[i, j] with
                | Enterprise _ -> 
                    if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                        quadrant.Sectors[i, j] <- sector
                    quadrant.Sectors.[i, j] <- createEmptySpace (i, j)
                | _ -> ()

                if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                    quadrant.Sectors[i, j] <- copyEnterprise { state.Enterprise with 
                                                                Energy = initialEnergy; 
                                                                ShieldStrength = initialShieldStrength; 
                                                                Torpedoes = initialTorpedoes; 
                                                                Condition = "DOCKED";
                                                                SectorId = (i,j)
                                                                }

        { state with CurrentSector = (fst state.CurrentSector + x1, snd state.CurrentSector + x2); }

    let moveEnterprise state quadrant =
        for i in sectorRange do
            for j in sectorRange do
                match quadrant.Sectors[i, j] with
                | Enterprise _ -> 
                    quadrant.Sectors[i, j] <- createEmptySpace (i, j)
                | _ -> ()

                if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                    quadrant.Sectors[i, j] <- copyEnterprise { state.Enterprise with SectorId = (i,j)}

        { state with CurrentSector = (fst state.CurrentSector + x1, snd state.CurrentSector + x2) }

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
        | _ ->  let s1 = moveEnterprise state quadrant 
                s1
                

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


let longRangeScan (state : State) =
    let getQuadrant (state : State) (qadrantId : (int * int)) : Quadrant option =
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

let setDevice enterprise i value =
    match i with
    | 1 -> { enterprise with WarpEngines = value }
    | 2 -> { enterprise with ShortRangeSensors = value }
    | 3 -> { enterprise with LongRangeSensors = value }
    | 4 -> { enterprise with ShieldControl = value }
    | 5 -> { enterprise with LibraryComputer = value }
    | 6 -> { enterprise with PhaserControl = value }
    | 7 -> { enterprise with PhotonTubes = value }
    | 8 -> { enterprise with DamageControl = value }
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
    if state.Enterprise.Energy < initialEnergy / 10 then
        "YELLOW"
    else if quadrant.Klingons > 0 then
        "*RED*"
    else
        "GREEN"

(* LINE 6000 *)
let klingonsShooting state =
    let quadrant = currentQuadrant state 
    let mutable shieldUnits = state.Enterprise.ShieldStrength
    let mutable enterprise = state.Enterprise

    if quadrant.Klingons > 0 then
        if state.Enterprise.ShieldStrength <= 0 then

            for i in sectorRange do
                for j in sectorRange do
                    match quadrant.Sectors[i, j] with
                    | Klingon k -> 
                        let distance = sqrt (double ((square (fst k.SectorId - fst state.CurrentSector)) + (square (snd k.SectorId - snd state.CurrentSector))))
                        let tmp = distance * (rnd.NextDouble() + 2.0)
                        let h = double k.ShieldStrength / tmp
                        shieldUnits <- shieldUnits - int h
                        quadrant.Sectors[i, j] <- copyKlingon { k with ShieldStrength = int (double k.ShieldStrength / 3.0 * rnd.NextDouble()) }
                        printfn $"{h} UNIT HIT ON ENTERPRISE FROM SECTOR {fst k.SectorId},{snd k.SectorId}"
                        if shieldUnits > 0 then
                            printfn $"   <SHIELDS DOWN TO {shieldUnits} UNITS>"
                            if h > 20 then
                                if rnd.NextDouble() > 0.6 || h / double shieldUnits <= 0.02 then
                                    ()
                                else
                                    let r1 = fnr
                                    let v = int (h / double shieldUnits - 0.5 * rnd.NextDouble())
                                    enterprise <- setDevice state.Enterprise r1 v
                                    printfn  $"DAMAGE CONTROL REPORTS {getDeviceName r1} DAMAGED BY THE HIT'"
                    | _ -> ()
        else
            printf "STARBASE SHIELDS PROTECT THE ENTERPRISE"
        
    { state with Enterprise = { enterprise with ShieldStrength = shieldUnits; Condition = getCondition state } }

let firePhasers (state : State) =
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
            printfn "SCIENCE OFFICER SPOCK REPORTS  'SENSORS SHOW NO ENEMY SHIPS"
            printfn "                                IN THIS QUADRANT'"
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
                        let distance = sqrt (double ((square (fst k.SectorId - fst state.CurrentSector)) + (square (snd k.SectorId - snd state.CurrentSector))))
                        let h = (h1 / distance) * (rnd.NextDouble() + 2.0)
                    
                        if h <= 0.15 * double k.ShieldStrength then
                            printfn $"SENSORS SHOW NO DAMAGE TO ENEMY AT {fst k.SectorId},{snd k.SectorId}"
                        else
                            printfn $"{int h} UNIT HIT ON KLINGON AT SECTOR {fst k.SectorId}, {snd k.SectorId}"
                            if k.ShieldStrength - int h < 0 then
                                printfn "**** KLINGON DESTROYED ****"
                                quadrant.Sectors[i, j]  <- createEmptySpace (i, j)
                                state.Galaxy.Quadrants[fst state.CurrentQuadrant, snd state.CurrentQuadrant] <- { quadrant with Klingons = quadrant.Klingons - 1 }
                                klingonsDestroyed <- klingonsDestroyed + 1
                            else
                                printfn $"   (SENSORS SHOW {k.ShieldStrength} UNITS REMAINING)"
                                quadrant.Sectors[i, j]  <- copyKlingon { k with ShieldStrength = k.ShieldStrength - int h }
                    | _ -> ()

    shortRangeScan (klingonsShooting { state with 
                                        TotalKlingons = (state.TotalKlingons - klingonsDestroyed); 
                                        Enterprise = { 
                                            state.Enterprise with Energy = state.Enterprise.Energy - int energyUsed; Condition = getCondition state 
                                        } 
                                    })
            

let tor state =
    printfn "TOR"
    state

let she state =
    printfn "SHE"
    state

let dam state =
    printfn "DAM"
    state

let validNav state =
    printfn "NAVIGATION DIRECTIONS"
    printfn "====================="
    printfn "4   3   2"
    printfn "  \ ' /"
    printfn "5 - * - 1"
    printfn "  / ' \ "
    printfn "6   7   8"
    state

let computer(state : State) =
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
        st <- match getCommand() with
                | "1" -> computerStatusReport state
                | "2" -> computerPhotonTorpedoData state
                | "3" -> computerStarbaseData state
                | "4" -> directionDistanceCalculator state
                | "5" -> galaxyMap state
                | "6" -> validNav state
                | "7" -> isOk <- false; state
                | _ -> st;
    st

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
    state <- shortRangeScan state
    let mutable isOk = true
    
    while isOk do
        state <- match commandMenu() with
                 | "NAV" -> navigate state
                 | "SRS" -> shortRangeScan state
                 | "LRS" -> longRangeScan state
                 | "PHA" -> firePhasers state
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
