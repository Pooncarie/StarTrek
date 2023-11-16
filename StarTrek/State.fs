module State

open Domain
open Sector
open Quadrant
open Enterprise

type Galaxy = {
    Quadrants : Quadrant array2d 
    }

type State = {
    Galaxy : Galaxy
    Enterprise : Enterprise
    StarDate : double              // T
    NumberOfStarDays : int      // T9
    StartedOnStardate : double     // T0
    CurrentQuadrant : QuadrantId
    CurrentSector : SectorId
    TotalKlingons : int
    TotalStarbases : int
    TotalStars : int    
    DirectionArray : int array2d
    EndOfGameReason : Endings option
    }

let currentQuadrant state = state.Galaxy.Quadrants.[fst state.CurrentQuadrant, snd state.CurrentQuadrant]

let createState = 
    let createGalaxy = { Quadrants = Array2D.init maxQuadrants maxQuadrants (fun i j -> createQuadrant i j) }

    let arrayOfMove = [|
        [| 0; 1|]
        [| -1; 1|]
        [| -1; 0|]
        [| -1; -1|]
        [| 0; -1|]
        [| 1; -1|]
        [| 1; 0|]
        [| 1; 1|]
        [| 0; 1|]
        |]

    let tmpStarDate = initialStardate

    let s = {
        Galaxy = createGalaxy
        Enterprise = createEnterprise (0,0) 
        StarDate = tmpStarDate
        StartedOnStardate = tmpStarDate
        NumberOfStarDays = 25 + (int) (rnd.NextDouble() * 10.0)
        CurrentQuadrant = QuadrantId(0, 0)
        CurrentSector = SectorId(0,0)
        TotalKlingons = 0
        TotalStarbases = 0
        TotalStars = 0
        DirectionArray = Array2D.init 9 2 (fun i j -> arrayOfMove[i][j])
        EndOfGameReason = None
        }

    let mutable totalStarbases = 0
    let mutable totalKlingons = 0
    let mutable totalStars = 0

    s.Galaxy.Quadrants |> Array2D.iteri (fun i j quadrant ->
            totalStarbases <- totalStarbases + quadrant.Starbases
            totalKlingons <- totalKlingons + quadrant.Klingons
            totalStars <- totalStars + quadrant.Stars
        )

    { s with TotalKlingons = totalKlingons; TotalStarbases = totalStarbases; TotalStars = totalStars;  }


let getKlingons state =
    let mutable klingons = []
    let quadrant = currentQuadrant state

    quadrant.Sectors |> Array2D.iteri (fun i j sector ->
            match sector with
            | Klingon k -> klingons <- k :: klingons
            | _ -> ()
        )

    klingons

let getStarbases state = 
    let mutable starbases = []
    let quadrant = currentQuadrant state

    quadrant.Sectors |> Array2D.iteri (fun i j sector ->
            match sector with
            | Starbase s -> starbases <- s :: starbases
            | _ -> ()
        )

    starbases

