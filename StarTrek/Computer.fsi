module Computer
open Domain

val private distanceCalculator: coords: DistanceCoordinates -> unit

val private computerStatusReport: state: State -> unit

val private computerPhotonTorpedoData: state: State -> unit

val private computerStarbaseData: state: State -> unit

val private directionDistanceCalculator: state: State -> unit

val private galaxyMap: state: State -> unit

val private validNav: unit

val computer: state: State -> State

