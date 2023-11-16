module Starbase

open Domain

[<Struct>]
type Starbase =  {
    SectorId : SectorId
}

let createStarbase sectorId = { 
    Starbase.SectorId = sectorId; 
    }
