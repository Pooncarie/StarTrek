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

let readLine() = Console.ReadLine().Trim().ToUpper();

let square x = x * x

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
        Sectors = Array2D.init 8 8 (fun i j -> createEmptySpace (i, j))
    }

    quadrant

let createState = 
    let createGalaxy = { Quadrants = Array2D.init 8 8 (fun i j -> quadrantCreate i j) }

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
        Galaxy = Some createGalaxy
        Enterprise = None
        StarDate = int (rnd.NextDouble() * 20.0 + 20.0) * 100
        NumberOfStarDays = 25 + (int) (rnd.NextDouble() * 10.0)
        CurrentQuadrant = QuadrantId(0, 0)
        CurrentSector = SectorId(0,0)
        TotalKlingons = 0
        TotalStarbases = 0
        TotalStars = 0
        EngineDamage = 0
        SRS_Damage = 0
        LRS_Damage = 0
        PhasersDamage = 0
        ComputerDamage = 0
        DeflectorDamage = 0        
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

    { s with TotalKlingons = totalKlingons; TotalStarbases = totalStarbases; TotalStars = totalStars;  }


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

    let printIt (s : Sector)  =
        match s with
        | Klingon k -> printf $" {k.Symbol}"
        | Enterprise e -> printf $" {e.Symbol}"
        | Star s -> printf $" {s.Symbol}"
        | Starbase s -> printf $" {s.Symbol}"
        | EmptySpace e -> printf $" {e.Symbol}"

    let sector = state.Galaxy.Value.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant].Sectors;

    for i in [0..7] do 
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

    match state.Enterprise with
    | Some enterprise ->
        printfn $"SHORT RANGE SCAN FOR QUADRANT {(quadrantName state.CurrentQuadrant)}"
        printfn "   "
        printfn " +--1---2---3---4---5---6---7---8-+"
        printf "1|"
        printSector state 0; printf "|1";
        printfn $"        STARDATE            {state.StarDate}"
        printf "2|"
        printSector state 1; printf "|2";
        printf $"        CONDITION           "; printCondition enterprise; printfn ""
        printf "3|"
        printSector state 2; printf "|3";
        printfn $"        QUADRANT            {fst state.CurrentQuadrant + 1},{snd state.CurrentQuadrant + 1}"
        printf "4|"
        printSector state 3; printf "|4";
        printfn $"        SECTOR              {fst state.CurrentSector + 1},{snd state.CurrentSector + 1}"
        printf "5|"
        printSector state 4; printf "|5";
        printfn $"        PHOTO TORPEDOES     {enterprise.Torpedoes}"
        printf "6|"
        printSector state 5; printf "|6";
        printfn $"        TOTAL ENERGY        {enterprise.Energy + enterprise.ShieldStrength}"
        printf "7|"
        printSector state 6; printf "|7";
        printfn $"        SHIELDS             {enterprise.ShieldStrength}"
        printf "8|"
        printSector state 7; printf "|8";
        printfn $"        KLINGONS REMAINING  {state.TotalKlingons}"
        printf " +--------------------------------+"
        printfn "   "
        state
    | _ -> state

let changeQuadrant state newQuadrant : State =

    let clearSectors quadrant = 
        for i in [0..7] do
            for j in [0..7] do
                quadrant.Sectors[i, j] <- createEmptySpace (i,j)

    let sectorIndexs = Seq.initInfinite (fun _ -> (rnd.Next(0, 7), rnd.Next(0, 7))) |> Seq.distinct |> Seq.take(40) |> Seq.toArray
    let quadrant = state.Galaxy.Value.Quadrants.[fst newQuadrant, snd newQuadrant]

    clearSectors quadrant
    let mutable cnt = 0

    printfn ""
    printfn $"NOW ENTERING {quadrantName newQuadrant} QUADRANT . . ."

    (* 
        TODO: Currently putting Enterprize into a new sector BUT I think it should be in the sector the user 
        has navigated to. This will is ok for the moment.
    *)
    match state.Enterprise with
    | Some enterprise -> 
        if quadrant.Klingons > 0 then
            printfn "COMBAT AREA **** CONDITION RED"
            if (enterprise.ShieldStrength < 200) then
                printfn "SHIELDS DANGEROUSLY LOW"
            quadrant.Sectors[fst sectorIndexs[0], snd sectorIndexs[0]] <- copyEnterprise { enterprise with Condition = "*RED*"; SectorId =  sectorIndexs[cnt] }
        else
            quadrant.Sectors[fst sectorIndexs[0], snd sectorIndexs[0]] <- copyEnterprise { enterprise with SectorId =  sectorIndexs[cnt]; Condition = "GREEN" }

    | None -> 
        let newEnterprise = createEnterprise (fst sectorIndexs[cnt], snd sectorIndexs[cnt])
        if quadrant.Klingons > 0 then
            printfn "COMBAT AREA **** CONDITION RED"
            printfn "SHIELDS DANGEROUSLY LOW"
            match newEnterprise with
            | Enterprise e -> quadrant.Sectors[fst sectorIndexs[0], snd sectorIndexs[0]] <- copyEnterprise { e with Condition = "*RED*"; }
            | _ -> ()

        else
            quadrant.Sectors[fst sectorIndexs[0], snd sectorIndexs[0]] <- newEnterprise
            
    cnt <- cnt + 1

    // TODO: THIS IS JUST FOR TESTING
    //quadrant.Sectors.[fst sectorIndexs[cnt], snd sectorIndexs[cnt]] 
    //        <- createStarbase  (fst sectorIndexs[cnt], snd sectorIndexs[cnt])
    //cnt <- cnt + 1

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
        Enterprise = match quadrant.Sectors[fst sectorIndexs[0], snd sectorIndexs[0]] with | Enterprise e -> Some e | _ -> None
        }



let startGame (state : State) =
    (* Start in a random quadrant *)
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
    for i in [0..7] do
        printf "| "
        for j in [0..7] do
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
    let quadrant = state.Galaxy.Value.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant];

    for i in [0..7] do
        for j in [0..7] do
            match quadrant.Sectors[i, j] with
            | Klingon k -> klingons <- k :: klingons
            | _ -> ()
    klingons

let getStarbases state = 
    let mutable starbases = []
    let quadrant = state.Galaxy.Value.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant];

    for i in [0..7] do
        for j in [0..7] do
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
            printf "FROM ENTERPRISE TO STARBASE "; 
            distanceCalculator { 
                InitialX = fst state.CurrentSector + 1;
                InitialY = snd state.CurrentSector + 1; 
                FinalX = fst starbase.SectorId + 1; 
                FinalY = snd starbase.SectorId + 1})

    state

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
            match state.Enterprise with
            | Some enterprise ->
                if enterprise.Energy - n >= 0 then
                    Some n
                else
                    printfn "ENGINEERING REPORTS   'INSUFFICIENT ENERGY AVAILABLE"
                    printfn $"                     FOR MANEUVERING AT WARP {warpFactor}!'"
               
                    if enterprise.ShieldStrength < n - enterprise.Energy || state.DeflectorDamage < 0 then
                        None
                    else
                        printfn $"DEFLECTOR CONTROL ROOM ACKNOWLEDGES  {enterprise.ShieldStrength} UNITS OF ENERGY"
                        printfn "                         PRESENTLY DEPLOYED TO SHIELDS."
                        None
            | _ -> None
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
           
        match state.Enterprise with
        | Some enterprise ->
            for i in [0..7] do
                for j in [0..7] do
                    match quadrant.Sectors.[i, j] with
                    | Enterprise _ -> 
                        if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                            quadrant.Sectors[i, j] <- sector
                        quadrant.Sectors.[i, j] <- createEmptySpace (i, j)
                    | _ -> ()

                    if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                        quadrant.Sectors[i, j] <- copyEnterprise { enterprise with 
                                                                    Energy = initialEnergy; 
                                                                    ShieldStrength = initialShieldStrength; 
                                                                    Torpedoes = initialTorpedoes; 
                                                                    Condition = "DOCKED";
                                                                    SectorId = (i,j)
                                                                    }
        | _ -> ()

        { state with CurrentSector = (fst state.CurrentSector + x1, snd state.CurrentSector + x2); }

    let moveEnterprise state quadrant =
       match state.Enterprise with
       | Some enterprise ->
            for i in [0..7] do
                for j in [0..7] do
                    match quadrant.Sectors[i, j] with
                    | Enterprise _ -> 
                        quadrant.Sectors[i, j] <- createEmptySpace (i, j)
                    | _ -> ()

                    if i = fst state.CurrentSector + x1 && j = snd state.CurrentSector + x2 then
                        quadrant.Sectors[i, j] <- copyEnterprise { enterprise with SectorId = (i,j)}
       | _ -> ()

       { state with CurrentSector = (fst state.CurrentSector + x1, snd state.CurrentSector + x2) }

    if fst state.CurrentSector + x1 < 0 || fst state.CurrentSector + x1 > 7 || snd state.CurrentSector + x2 < 0 || snd state.CurrentSector + x2 > 7 then
        navigateQuadrant state x1 x2
    else
        let quadrant = state.Galaxy.Value.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant]
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

let firePhasers (state : State) =
    let getPhaserUnits =
        let mutable ok = false
        let mutable noOfUnits = 0.0
        let energy = match state.Enterprise with | Some e -> e.Energy | _ -> 0

        while not ok do
            printfn $"ENERGY AVAILABLE = {energy} UNITS"

            noOfUnits <- inputDouble "NUMBER OF UNITS TO FIRE: "

            if noOfUnits > 0 && noOfUnits < energy then
                ok <- true

        noOfUnits

    if state.PhasersDamage < 0 then
        printfn "PHASER CONTROL IS INOPERABLE."
    else
        let klingons = getKlingons state

        if klingons.Length = 0 then 
            printfn ""
            printfn "SCIENCE OFFICER SPOCK REPORTS  'SENSORS SHOW NO ENEMY SHIPS"
            printfn "                                IN THIS QUADRANT'"
        else 
            if state.ComputerDamage < 0 then
                printfn "COMPUTER FAILURE HAMPERS ACCURACY"

            printfn "PHASERS LOCKED ON TARGET;  "
           
               
            if state.DeflectorDamage < 0 then
                let h1 = double (int ((double getPhaserUnits) * rnd.NextDouble()) / klingons.Length)
                for klingon in klingons do
                    let distance = sqrt (double ((square (fst klingon.SectorId - fst state.CurrentSector)) + (square (snd klingon.SectorId - snd state.CurrentSector))))
                    let h = (h1 / distance) * (rnd.NextDouble() + 2.0)
                    
                    if h > 0.15 then
                        ()
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
