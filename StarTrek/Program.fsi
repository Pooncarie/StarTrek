module Program
open Domain
open Computer
open Input

val start: unit -> unit

val endOfGame: state: State -> reason: Endings -> State

val checkForFatalErrors: state: State -> unit

val shortRangeScan: state: State -> State

val changeQuadrant: state: State -> int * int -> State

val startGame: state: State -> State

val getCourse: unit -> double option

val getWarp: state: State -> (int * double) option

val navigateQuadrant: state: State -> x: int -> y: int -> State

val getDecimalPart: num: double -> double

val getDevice: enterprise: Enterprise -> i: int -> double

val setDevice:
  enterprise: Enterprise -> i: int -> value: double -> Enterprise

val getDeviceName: i: int -> string

val getCondition: state: State -> Condition

val klingonsShooting: state: State -> State

val navigateSector: state: State -> course: double -> State

val navigate: state: State -> State

val longRangeScan: state: State -> State

val firePhasers: state: State -> State

val photonTorpedoes: state: State -> State

val shieldControl: state: State -> State

val damageControl: state: State -> State

val mainLoop: unit -> bool

val main: argv: string array -> int


