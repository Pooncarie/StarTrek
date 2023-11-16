module EmptySpace

open Domain

[<Struct>]
type EmptySpace =
    { SectorId: SectorId }

val createEmptySpace: int * int -> EmptySpace
