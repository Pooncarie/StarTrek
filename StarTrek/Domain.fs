module Domain

open System

let maxQuadrants = 8
let maxSectors = maxQuadrants
let quadrantRange = [0..maxQuadrants-1]
let sectorRange = [0..maxSectors-1]

let rnd = Random();
let initialEnergy = 3000
let initialShieldStrength = 0
let initialTorpedoes = 10
let initialKlingonShieldStrength = int (200.0 * (0.5 + rnd.NextDouble())); 
let initialStardate = (rnd.NextDouble() * 20.0 + 20.0) * 100.0
let fnr = int (rnd.NextDouble() * 7.98 + 1.01)
let square (x : double) = x * x

type Condition = Green | Yellow | Red | Docked
type Endings = Destroyed | Won | Quit | TooLong | NoEnergy | FatalError
type SectorId = int * int
type QuadrantId = int * int

type DistanceCoordinates = {
    InitialX : double
    InitialY : double
    FinalX : double
    FinalY : double
    }



