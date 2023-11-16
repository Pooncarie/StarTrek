module Computer
open Domain
open State

val private getDirection: deltaX: double -> deltaY: double -> double

val getDistance: deltaX: double -> deltaY: double -> double

val private remove00: str: string -> string 

val private doDirectionCalculations: coords: DistanceCoordinates -> string

val private doDistanceCalculations: coords: DistanceCoordinates -> string

val private getDirectionDistance: initCoordinate: (float * float) option -> finalCoordinate: (float * float) option -> string list

val private directionDistanceCalculator: state: State -> string list

val private computerPhotonTorpedoData: state: State -> string list

val private computerStarbaseData: state: State -> string list

val private validNav: state: State -> string list

val private galaxyMap: state: State -> string list

val private computerStatusReport: state: State -> string list

val computer: state: State -> State
