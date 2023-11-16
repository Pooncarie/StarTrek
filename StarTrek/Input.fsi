module Input

open Menu

val private readLine: unit -> string
val private getInteger: lineReader: (unit -> string)  -> int option
val private getDouble: lineReader: (unit -> string)  -> double option
val private getDoubleInRange: lineReader: (unit -> string) -> range: double list -> double option
val private getStringInRange: lineReader: (unit -> string) -> range: string list -> string option 

val inputInteger: prompt: string -> int

val inputDouble: prompt: string -> float

val inputDoubleInRange: prompt: string -> range: float list -> float

val inputString: prompt: string -> string

val inputCoordinate: prompt: string -> (float * float) option

val inputValidMenuOption: prompt: string -> commands: Menu list -> Menu

val inputValidMenuOption2: prompt: string -> commands: Menu2 list -> Menu2

