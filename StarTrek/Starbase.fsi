module Starbase

open Domain

[<Struct>]
type Starbase =  {
    SectorId : SectorId
    Symbol : string
}

val createStarbase: int * int -> Starbase
