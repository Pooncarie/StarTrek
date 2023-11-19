module Starbase

open Domain

[<Literal>]
let private symbol = ">!<"

[<Struct>]
type Starbase =  {
    SectorId : SectorId
    Symbol : string
}

let createStarbase sectorId = { 
    Starbase.SectorId = sectorId; 
    Symbol = symbol
    }
