module Sector

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


let createEmptySpace sectorId = EmptySpace (EmptySpace.createEmptySpace sectorId)
let createKlingon sectorId = Klingon (Klingon.createKlingon sectorId)
let createStar sectorId = Star (Star.createStar sectorId)
let createStarbase sectorId = Starbase (Starbase.createStarbase sectorId)
let createEnterprise sectorId = Enterprise (Enterprise.createEnterprise sectorId)

let copyEnterprise (enterprise : Enterprise) = Enterprise (Enterprise.copyEnterprise enterprise)
let copyKlingon (klingon : Klingon) = Klingon (Klingon.copyKlingon klingon)
