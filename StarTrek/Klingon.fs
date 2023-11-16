module Klingon

open Domain

[<Struct>]
type Klingon = {
    SectorId : SectorId
    ShieldStrength : int
}

let createKlingon sectorId =  { 
    SectorId = sectorId; 
    ShieldStrength =  initialKlingonShieldStrength 
    }

let copyKlingon (klingon : Klingon) = { 
    SectorId = klingon.SectorId; 
    ShieldStrength =  klingon.ShieldStrength 
    }