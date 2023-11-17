module Menu2

open State

type Menu2 = {
    Key : string
    Text : string
    Function: State -> string list
    Exit : bool
    }

val inputValidMenuOption2: prompt: string -> commands: Menu2 list -> Menu2
