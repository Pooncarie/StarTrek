module Klingon
open Domain

[<Struct>]
type Klingon =
    {
      SectorId: SectorId
      ShieldStrength: int
    }

val createKlingon: int * int -> Klingon

val copyKlingon: klingon: Klingon -> Klingon
