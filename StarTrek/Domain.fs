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

type SectorId = int * int
type QuadrantId = int * int

type Klingon = {
    SectorId : SectorId
    ShieldStrength : int
    Symbol : string
}

type Enterprise = {
    SectorId : SectorId
    Condition : string
    Symbol : string
    Energy : int
    ShieldStrength : int
    Torpedoes : int
    WarpEngines : int                   // D(1)
    ShortRangeSensors : int             // D(2)
    LongRangeSensors : int              // D(3)
    PhaserControl : int                 // D(4) 
    PhotonTubes : int                   // D(5)
    DamageControl : int                 // D(6)
    ShieldControl : int                 // D(7)
    LibraryComputer : int               // D(8)
}

type Star = {
    SectorId : SectorId
    Symbol : string
}

type Starbase =  {
    SectorId : SectorId
    Symbol : string
}

type EmptySpace = {
    SectorId : SectorId
    Symbol : string;
}

type Sector = Klingon of Klingon | Enterprise of Enterprise | Star of Star | Starbase of Starbase| EmptySpace of EmptySpace

let createKlingon sectorId = Klingon { 
    SectorId = sectorId; 
    ShieldStrength =  initialKlingonShieldStrength 
    Symbol = "<K>";
    }

let copyKlingon (klingon : Klingon) = Klingon { 
    SectorId = klingon.SectorId; 
    ShieldStrength =  klingon.ShieldStrength 
    Symbol = "<K>";
    }

let createEnterprise sectorId : Enterprise = { 
    SectorId = sectorId; 
    Symbol = "<E>"; 
    Condition = "GREEN" ; 
    Energy = initialEnergy;
    ShieldStrength = initialShieldStrength;
    Torpedoes = initialTorpedoes
    WarpEngines = 0
    ShortRangeSensors = 0
    LongRangeSensors = 0
    PhaserControl = 0
    ShieldControl = 0
    PhotonTubes = 0
    LibraryComputer = 0
    DamageControl = 0
    }

let copyEnterprise (enterprise : Enterprise)  = Enterprise { 
    SectorId = enterprise.SectorId; 
    Symbol = "<E>"; 
    Condition = enterprise.Condition; 
    Energy = enterprise.Energy;
    ShieldStrength = enterprise.ShieldStrength
    Torpedoes = enterprise.Torpedoes
    WarpEngines = enterprise.WarpEngines
    ShortRangeSensors = enterprise.ShortRangeSensors
    LongRangeSensors = enterprise.LongRangeSensors
    ShieldControl = enterprise.ShieldControl
    LibraryComputer = enterprise.LibraryComputer
    PhaserControl = enterprise.PhaserControl
    PhotonTubes = enterprise.PhotonTubes
    DamageControl = enterprise.DamageControl
    }

let createStar sectorId = Star { 
    Star.SectorId = sectorId; 
    Symbol = " * ";
    }

let createStarbase sectorId = Starbase { 
    Starbase.SectorId = sectorId; 
    Symbol = ">!<";
    }

let createEmptySpace sectorId = EmptySpace { 
    EmptySpace.SectorId = sectorId; 
    Symbol = "   ";
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
    InitialX : int
    InitialY : int
    FinalX : int
    FinalY : int
    }

type State = {
    Galaxy : Galaxy
    Enterprise : Enterprise
    StarDate : int
    NumberOfStarDays : int
    CurrentQuadrant : QuadrantId
    CurrentSector : SectorId
    TotalKlingons : int
    TotalStarbases : int
    TotalStars : int    
    DirectionArray : int array2d
    StartAgain : bool
    Error : bool
    }


