module Star
open Domain

[<Struct>]
type Star =
    { 
        SectorId: SectorId 
        Symbol : string
    }

val createStar: int * int -> Star