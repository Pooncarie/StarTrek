module Klingon

open Domain

[<Literal>]
let private symbol = "<K>"

[<Struct>]
type Klingon = {
    SectorId : SectorId
    ShieldStrength : int
    Symbol : string
}

let createKlingon sectorId =  { 
    SectorId = sectorId; 
    ShieldStrength =  initialKlingonShieldStrength 
    Symbol = symbol
    }

let copyKlingon klingon = { 
    SectorId = klingon.SectorId; 
    ShieldStrength =  klingon.ShieldStrength 
    Symbol = klingon.Symbol
    }