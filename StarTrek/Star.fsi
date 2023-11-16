module Star
open Domain

[<Struct>]
type Star =
    { SectorId: SectorId }

val createStar: int * int -> Star