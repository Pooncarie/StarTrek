module Star

open Domain

[<Literal>]
let private symbol = " * "

[<Struct>]
type Star =
    { 
        SectorId: SectorId 
        Symbol : string
    }

let createStar sectorId = { 
    Star.SectorId = sectorId; 
    Symbol = symbol
    }


