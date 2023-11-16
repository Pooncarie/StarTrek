module Sector

open Domain
open Klingon
open Enterprise
open Star
open Starbase
open EmptySpace

type Sector =
    | Klingon of Klingon
    | Enterprise of Enterprise
    | Star of Star
    | Starbase of Starbase
    | EmptySpace of EmptySpace

val createEmptySpace: sectorId: SectorId -> Sector
val createKlingon: sectorId: SectorId -> Sector
val createStar: sectorId: SectorId -> Sector 
val createStarbase: sectorId: SectorId -> Sector
val createEnterprise: sectorId: SectorId -> Sector

val copyEnterprise: Enterprise -> Sector
val copyKlingon: Klingon -> Sector