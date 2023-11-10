module Input
open Domain
val private readLine: unit -> string

val inputInteger: prompt: string -> int

val inputDouble: prompt: string -> float

val inputDoubleInRange: prompt: 'a -> range: float list -> float

val inputString: prompt: string -> string

val inputCoordinate: prompt: string -> float * float

val inputValidMenuOption: prompt: string -> commands: Menu list -> Menu

