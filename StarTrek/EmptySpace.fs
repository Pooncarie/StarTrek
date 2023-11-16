module EmptySpace

open Domain

[<Struct>]
type EmptySpace =
    { SectorId: SectorId }


let createEmptySpace sectorId = { 
    EmptySpace.SectorId = sectorId; 
    }