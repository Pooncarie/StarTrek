module Starbase

open Domain

[<Struct>]
type Starbase =  {
    SectorId : SectorId
}

val createStarbase: int * int -> Starbase
