module Program
open Domain
open Enterprise
open State
open Quadrant

val printList: list: string list -> unit

val getEndOfGameText: state: State -> string list

val startGameText: state: State -> newState: State -> string list

val endOfGame: state: State -> State

val checkForFatalErrors: state: State -> unit

val shortRangeScan: state: State -> State

val changeQuadrant: state: State -> int * int -> State

val startGame: state: State -> State

val getCourse: unit -> double option

val getWarp: state: State -> (int * double) option

val useShieldEnergy: state: State -> warpFactor: double -> State

val navigateQuadrant: state: State -> x: int -> y: int -> warpFactor: double -> State

val getDecimalPart: num: double -> double

val getCondition: state: State -> Condition

val klingonsShooting: state: State -> State

val navigateSector: state: State -> course: double -> warpFactor : double -> State

val navigate: state: State -> State

val longRangeScan: state: State -> State

val private getKlingonsDestroyed: state: State -> quadrant: Quadrant ->  energy: double -> int

val firePhasers: state: State -> State

val photonTorpedoes: state: State -> State

val shieldControl: state: State -> State

val damageControl: state: State -> string list

val mainLoop: unit -> unit

val main: argv: string array -> int


