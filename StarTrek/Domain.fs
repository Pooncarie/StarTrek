module Domain

open System

let maxQuadrants = 8
let maxSectors = maxQuadrants
let quadrantRange = [0..maxQuadrants-1]
let sectorRange = [0..maxSectors-1]

let rnd = Random();
let initialEnergy = 3000
let initialShieldStrength = 0
let initialTorpedoes = 10
let initialKlingonShieldStrength = int (200.0 * (0.5 + rnd.NextDouble())); 
let initialStardate = int (rnd.NextDouble() * 20.0 + 20.0) * 100
let fnr = int (rnd.NextDouble() * 7.98 + 1.01)
let square (x : double) = x * x

type Condition = Green | Yellow | Red | Docked
type Endings = Destroyed | Won | Quit | TooLong | NoEnergy | FatalError
type SectorId = int * int
type QuadrantId = int * int

[<Struct>]
type Klingon = {
    SectorId : SectorId
    ShieldStrength : int
}

type Enterprise = {
    SectorId : SectorId
    Condition : Condition
    Energy : int                            // E
    ShieldEnergy : int                      // S
    Torpedoes : int                         // P
    WarpEngines : double                    // D(1)
    ShortRangeSensors : double              // D(2)
    LongRangeSensors : double               // D(3)
    PhaserControl : double                  // D(4) 
    PhotonTubes : double                    // D(5)
    DamageControl : double                  // D(6)
    ShieldControl : double                  // D(7)
    LibraryComputer : double                // D(8)
    IsDocked : bool
}

[<Struct>]
type Star = {
    SectorId : SectorId
}

[<Struct>]
type Starbase =  {
    SectorId : SectorId
}

[<Struct>]
type EmptySpace = {
    SectorId : SectorId
}

type Sector = Klingon of Klingon | Enterprise of Enterprise | Star of Star | Starbase of Starbase| EmptySpace of EmptySpace

let createKlingon sectorId = Klingon { 
    SectorId = sectorId; 
    ShieldStrength =  initialKlingonShieldStrength 
    }

let copyKlingon (klingon : Klingon) = Klingon { 
    SectorId = klingon.SectorId; 
    ShieldStrength =  klingon.ShieldStrength 
    }

let createEnterprise sectorId = Enterprise  { 
    SectorId = sectorId; 
    Condition = Condition.Green ; 
    Energy = initialEnergy;
    ShieldEnergy = initialShieldStrength;
    Torpedoes = initialTorpedoes
    WarpEngines = 0
    ShortRangeSensors = 0
    LongRangeSensors = 0
    PhaserControl = 0
    ShieldControl = 0
    PhotonTubes = 0
    LibraryComputer = 0
    DamageControl = 0
    IsDocked = false
    }

let copyEnterprise (enterprise : Enterprise)  = Enterprise { 
    SectorId = enterprise.SectorId; 
    Condition = enterprise.Condition; 
    Energy = enterprise.Energy;
    ShieldEnergy = enterprise.ShieldEnergy
    Torpedoes = enterprise.Torpedoes
    WarpEngines = enterprise.WarpEngines
    ShortRangeSensors = enterprise.ShortRangeSensors
    LongRangeSensors = enterprise.LongRangeSensors
    ShieldControl = enterprise.ShieldControl
    LibraryComputer = enterprise.LibraryComputer
    PhaserControl = enterprise.PhaserControl
    PhotonTubes = enterprise.PhotonTubes
    DamageControl = enterprise.DamageControl
    IsDocked = enterprise.IsDocked
    }

let createStar sectorId = Star { 
    Star.SectorId = sectorId; 
    }

let createStarbase sectorId = Starbase { 
    Starbase.SectorId = sectorId; 
    }

let createEmptySpace sectorId = EmptySpace { 
    EmptySpace.SectorId = sectorId; 
    }

type Quadrant = {
    Starbases : int
    Stars : int
    Klingons : int
    QuadrantId : QuadrantId
    Sectors : Sector array2d
}

type Galaxy = {
    Quadrants : Quadrant array2d 
    }

type DistanceCoordinates = {
    InitialX : double
    InitialY : double
    FinalX : double
    FinalY : double
    }

type State = {
    Galaxy : Galaxy
    Enterprise : Enterprise
    StarDate : int              // T
    NumberOfStarDays : int      // T9
    StartedOnStardate : int     // T0
    CurrentQuadrant : QuadrantId
    CurrentSector : SectorId
    TotalKlingons : int
    TotalStarbases : int
    TotalStars : int    
    DirectionArray : int array2d
    EndOfGameReason : Endings option
    }

type Menu = {
    Command : string
    Text : string
    Function: State -> State
    Exit : bool
    }

let createQuadrant x y =

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


let currentQuadrant state = state.Galaxy.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant]

let getKlingons state =
    let mutable klingons = []
    let quadrant = currentQuadrant state

    quadrant.Sectors |> Array2D.iteri (fun i j sector ->
            match sector with
            | Klingon k -> klingons <- k :: klingons
            | _ -> ()
        )

    klingons

let getStarbases state = 
    let mutable starbases = []
    let quadrant = currentQuadrant state

    quadrant.Sectors |> Array2D.iteri (fun i j sector ->
            match sector with
            | Starbase s -> starbases <- s :: starbases
            | _ -> ()
        )

    starbases

let createState = 
    let createGalaxy = { Quadrants = Array2D.init maxQuadrants maxQuadrants (fun i j -> createQuadrant i j) }

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
        Enterprise = match createEnterprise (0,0) with | Enterprise e -> e | _ -> failwith "Enterprise not created"
        StarDate = tmpStarDate
        StartedOnStardate = tmpStarDate
        NumberOfStarDays = 25 + (int) (rnd.NextDouble() * 10.0)
        CurrentQuadrant = QuadrantId(0, 0)
        CurrentSector = SectorId(0,0)
        TotalKlingons = 0
        TotalStarbases = 0
        TotalStars = 0
        DirectionArray = Array2D.init 9 2 (fun i j -> arrayOfMove[i][j])
        EndOfGameReason = None
        }

    let mutable totalStarbases = 0
    let mutable totalKlingons = 0
    let mutable totalStars = 0

    s.Galaxy.Quadrants |> Array2D.iteri (fun i j quadrant ->
            totalStarbases <- totalStarbases + quadrant.Starbases
            totalKlingons <- totalKlingons + quadrant.Klingons
            totalStars <- totalStars + quadrant.Stars
        )

    { s with TotalKlingons = totalKlingons; TotalStarbases = totalStarbases; TotalStars = totalStars;  }
