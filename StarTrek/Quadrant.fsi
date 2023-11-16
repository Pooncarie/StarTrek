module Quadrant

open Domain
open Sector

type Quadrant =
    {
      Starbases: int
      Stars: int
      Klingons: int
      QuadrantId: QuadrantId
      Sectors: Sector array2d
    }

val createQuadrant: x: int -> y: int -> Quadrant

