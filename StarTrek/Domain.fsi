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

val initialStardate: int

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

[<Struct>]
type Klingon =
    {
      SectorId: SectorId
      ShieldStrength: int
    }

type Enterprise =
    {
      SectorId: SectorId
      Condition: Condition
      Energy: int
      ShieldEnergy: int
      Torpedoes: int
      WarpEngines: double
      ShortRangeSensors: double
      LongRangeSensors: double
      PhaserControl: double
      PhotonTubes: double
      DamageControl: double
      ShieldControl: double
      LibraryComputer: double
      IsDocked: bool
    }

[<Struct>]
type Star =
    { SectorId: SectorId }

[<Struct>]
type Starbase =
    { SectorId: SectorId }

[<Struct>]
type EmptySpace =
    { SectorId: SectorId }

type Sector =
    | Klingon of Klingon
    | Enterprise of Enterprise
    | Star of Star
    | Starbase of Starbase
    | EmptySpace of EmptySpace

val createKlingon: int * int -> Sector

val copyKlingon: klingon: Klingon -> Sector

val createEnterprise: int * int -> Sector

val copyEnterprise: enterprise: Enterprise -> Sector

val createStar: int * int -> Sector

val createStarbase: int * int -> Sector

val createEmptySpace: int * int -> Sector

type Quadrant =
    {
      Starbases: int
      Stars: int
      Klingons: int
      QuadrantId: QuadrantId
      Sectors: Sector array2d
    }

type Galaxy =
    { Quadrants: Quadrant array2d }

type DistanceCoordinates =
    {
      InitialX: double
      InitialY: double
      FinalX: double
      FinalY: double
    }

type State =
    {
      Galaxy: Galaxy
      Enterprise: Enterprise
      StarDate: int
      NumberOfStarDays: int
      StartedOnStardate: int
      CurrentQuadrant: QuadrantId
      CurrentSector: SectorId
      TotalKlingons: int
      TotalStarbases: int
      TotalStars: int
      DirectionArray: int array2d
    }

val createQuadrant: x: int -> y: int -> Quadrant

val currentQuadrant: state: State -> Quadrant

val getKlingons: state: State -> Klingon list

val getStarbases: state: State -> Starbase list

val createState: State



