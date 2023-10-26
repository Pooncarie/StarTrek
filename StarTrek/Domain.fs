module Domain

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

type DistanceCoordinates = {
    InitialX : int
    InitialY : int
    FinalX : int
    FinalY : int
    }

type State = {
    Galaxy : Galaxy option
    StarDate : int
    NumberOfStarDays : int
    Condition : string
    CurrentQuadrant : QuadrantId
    CurrentSector : SectorId
    Torpedoes : int
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


