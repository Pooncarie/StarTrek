module Klingon
open Domain

[<Struct>]
type Klingon =
    {
      SectorId: SectorId
      ShieldStrength: int
      Symbol : string
    }

val createKlingon: int * int -> Klingon

val copyKlingon: klingon: Klingon -> Klingon
