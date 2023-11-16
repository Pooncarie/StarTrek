module Star

open Domain

[<Struct>]
type Star =
    { SectorId: SectorId }

let createStar sectorId = { 
    Star.SectorId = sectorId; 
    }


