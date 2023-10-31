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
    EngineDamage : int                  // D(1)
    SRS_Damage : int                    // D(2)
    LRS_Damage : int                    // D(3)
    PhasersDamage : int                 // D(4) 
    DeflectorDamage : int               // D(7)
    ComputerDamage : int                // D(8)
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
    EngineDamage = 0
    SRS_Damage = 0
    LRS_Damage = 0
    PhasersDamage = 0
    DeflectorDamage = 0
    ComputerDamage = 0
    }

let copyEnterprise (enterprise : Enterprise)  = Enterprise { 
    SectorId = enterprise.SectorId; 
    Symbol = "<E>"; 
    Condition = enterprise.Condition; 
    Energy = enterprise.Energy;
    ShieldStrength = enterprise.ShieldStrength
    Torpedoes = enterprise.Torpedoes
    EngineDamage = enterprise.EngineDamage
    SRS_Damage = enterprise.SRS_Damage
    LRS_Damage = enterprise.LRS_Damage
    DeflectorDamage = enterprise.DeflectorDamage
    ComputerDamage = enterprise.ComputerDamage
    PhasersDamage = enterprise.PhasersDamage
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


