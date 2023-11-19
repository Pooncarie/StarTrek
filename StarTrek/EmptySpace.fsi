module EmptySpace

open Domain

[<Struct>]
type EmptySpace =
    { 
        SectorId: SectorId 
        Symbol : string
    }

val createEmptySpace: int * int -> EmptySpace
