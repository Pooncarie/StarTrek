module State

open Domain
open Quadrant
open Enterprise
open Klingon
open Starbase

type Galaxy =
    { Quadrants: Quadrant array2d }


type State =
    {
      Galaxy: Galaxy
      Enterprise: Enterprise
      StarDate: double
      NumberOfStarDays: int
      StartedOnStardate: double
      CurrentQuadrant: QuadrantId
      CurrentSector: SectorId
      TotalKlingons: int
      TotalStarbases: int
      TotalStars: int
      DirectionArray: int array2d
      EndOfGameReason: Endings option
    }

val getKlingons: state: State -> Klingon list

val getStarbases: state: State -> Starbase list

val createState: State

val currentQuadrant: state: State -> Quadrant