module Computer
open Domain


val private getDirection: deltaX: double -> deltaY: double -> double

val private getDistance: deltaX: double -> deltaY: double -> double

val private remove00: str: string -> string 

val private doCalculations: state: State -> coords: DistanceCoordinates -> unit

val private computerStatusReport: state: State -> State

val private computerPhotonTorpedoData: state: State -> State

val private computerStarbaseData: state: State -> State

val private directionDistanceCalculator: state: State -> State

val private galaxyMap: state: State -> State

val private validNav: state: State -> State

val computer: state: State -> State

