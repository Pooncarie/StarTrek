module EmptySpace

open Domain

[<Literal>]
let private symbol = "   "

[<Struct>]
type EmptySpace =
    { 
        SectorId: SectorId 
        Symbol : string
    }


let createEmptySpace sectorId = { 
    EmptySpace.SectorId = sectorId; 
    Symbol = symbol
    }