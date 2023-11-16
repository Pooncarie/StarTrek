module Menu

open State

type Menu = 
    {
      Command : string
      Text : string
      Function: State -> State
      Exit: bool
    }

type Menu2 = {
    Key : string
    Text : string
    Function: State -> string list
    Exit : bool
    }
