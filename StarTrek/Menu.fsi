module Menu

open State

type Menu = 
    {
      Command : string
      Text : string
      Function: State -> State
      Exit: bool
    }



val inputValidMenuOption: prompt: string -> commands: Menu list -> Menu

