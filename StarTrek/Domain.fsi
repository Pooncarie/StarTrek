module Domain
open System

val maxQuadrants: int

val maxSectors: int

val quadrantRange: int list

val sectorRange: int list

val rnd: Random

val initialEnergy: int

val initialShieldStrength: int

val initialTorpedoes: int

val initialKlingonShieldStrength: int

val initialStardate: double

val fnr: int

val square: x: double -> double

type Condition =
    | Green
    | Yellow
    | Red
    | Docked

type Endings =
    | Destroyed
    | Won
    | Quit
    | TooLong
    | NoEnergy
    | FatalError

type SectorId = int * int
type QuadrantId = int * int

type DistanceCoordinates =
    {
      InitialX: double
      InitialY: double
      FinalX: double
      FinalY: double
    }







