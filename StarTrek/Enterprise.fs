module Enterprise

open Domain

[<Literal>]
let private symbol = "<E>"

type Enterprise = {
    SectorId : SectorId
    Condition : Condition
    Energy : int                            // E
    ShieldEnergy : int                      // S
    Torpedoes : int                         // P
    WarpEngines : double                    // D(1)
    ShortRangeSensors : double              // D(2)
    LongRangeSensors : double               // D(3)
    PhaserControl : double                  // D(4) 
    PhotonTubes : double                    // D(5)
    DamageControl : double                  // D(6)
    ShieldControl : double                  // D(7)
    LibraryComputer : double                // D(8)
    IsDocked : bool
    Symbol : string
}

let createEnterprise sectorId =  { 
    SectorId = sectorId; 
    Condition = Condition.Green ; 
    Energy = initialEnergy;
    ShieldEnergy = initialShieldStrength;
    Torpedoes = initialTorpedoes
    WarpEngines = 0
    ShortRangeSensors = 0
    LongRangeSensors = 0
    PhaserControl = 0
    ShieldControl = 0
    PhotonTubes = 0
    LibraryComputer = 0
    DamageControl = 0
    IsDocked = false
    Symbol = symbol
    }

let copyEnterprise (enterprise : Enterprise)  = { 
    SectorId = enterprise.SectorId; 
    Condition = enterprise.Condition; 
    Energy = enterprise.Energy;
    ShieldEnergy = enterprise.ShieldEnergy
    Torpedoes = enterprise.Torpedoes
    WarpEngines = enterprise.WarpEngines
    ShortRangeSensors = enterprise.ShortRangeSensors
    LongRangeSensors = enterprise.LongRangeSensors
    ShieldControl = enterprise.ShieldControl
    LibraryComputer = enterprise.LibraryComputer
    PhaserControl = enterprise.PhaserControl
    PhotonTubes = enterprise.PhotonTubes
    DamageControl = enterprise.DamageControl
    IsDocked = enterprise.IsDocked
    Symbol = enterprise.Symbol
    }
