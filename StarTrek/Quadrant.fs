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

let createQuadrant x y =

    let klingonCount =
        match rnd.NextDouble() with
            | x when x > 0.98 -> 3
            | x when x > 0.95 -> 2
            | x when x > 0.80 -> 1
            | _ -> 0

    let starBaseCount =
        match rnd.NextDouble() with
            | x when x > 0.96 -> 1
            | _ -> 0

    let starCount =
        int  (rnd.NextDouble() * 7.98 + 1.01)

    let quadrant = {
        Quadrant.Starbases = starBaseCount
        Stars = starCount
        Klingons = klingonCount
        QuadrantId = (x, y)
        Sectors = Array2D.init maxSectors maxSectors (fun i j -> createEmptySpace (i, j))
    }

    quadrant


