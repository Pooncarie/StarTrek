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

(* In the basic version the devices are stored in an array D(), some of the calculations 
   iterate throught this array. Rather than have an array each device exists as a field
   in the Enterprise type, this lot of functions allowing mapping back and forth from
   the array to the device names in the Enterprise type *)
let getDevice enterprise deviceNo =
    match deviceNo with
    | 1 -> enterprise.WarpEngines
    | 2 -> enterprise.ShortRangeSensors
    | 3 -> enterprise.LongRangeSensors
    | 4 -> enterprise.PhaserControl
    | 5 -> enterprise.PhotonTubes
    | 6 -> enterprise.DamageControl
    | 7 -> enterprise.ShieldControl
    | 8 -> enterprise.LibraryComputer
    | _ -> 0

let setDevice enterprise deviceNo value =
    match deviceNo with
    | 1 -> { enterprise with WarpEngines = enterprise.WarpEngines + value }
    | 2 -> { enterprise with ShortRangeSensors = enterprise.ShortRangeSensors + value }
    | 3 -> { enterprise with LongRangeSensors = enterprise.LongRangeSensors + value }
    | 4 -> { enterprise with ShieldControl = enterprise.ShieldControl + value }
    | 5 -> { enterprise with LibraryComputer = enterprise.LibraryComputer + value }
    | 6 -> { enterprise with PhaserControl = enterprise.PhaserControl + value }
    | 7 -> { enterprise with PhotonTubes = enterprise.PhotonTubes + value }
    | 8 -> { enterprise with DamageControl = enterprise.DamageControl + value }
    | _ -> enterprise

let getDeviceName deviceNo =
    match deviceNo with
    | 1 -> "WARP ENGINES"
    | 2 -> "SHORT RANGE SENSORS"
    | 3 -> "LONG RANGE SENSORS"
    | 4 -> "PHASER CONTROL"
    | 5 -> "PHOTON TUBES"
    | 6 -> "DAMAGE CONTROL"
    | 7 -> "SHIELD CONTROL"
    | 8 -> "LIBRARY COMPUTER"
    | _ -> $"**** UNKOWN DEVICE {deviceNo} *****"
     
let printCondition enterprise =
    match enterprise.Condition with
    | Green -> "GREEN"
    | Yellow -> "YELLOW"
    | Red -> "*RED*"
    | Docked -> "DOCKED"

let getDamage enterprise : string list =
    [
        ""
        "DEVICE STATE OF REPAIR:"
        $"   WARP ENGINES = {enterprise.WarpEngines:N2}"
        $"   SHORT RANGE SENSORS = {enterprise.ShortRangeSensors:N2}"
        $"   LONG RANGE SENSORS = {enterprise.LongRangeSensors:N2}"
        $"   PHASER CONTROL = {enterprise.PhaserControl:N2}"
        $"   PHOTON TUBES = {enterprise.PhotonTubes:N2}"
        $"   DAMAGE CONTROL = {enterprise.DamageControl:N2}"
        $"   SHIELD CONTROL = {enterprise.ShieldControl:N2}"
        $"   LIBRARY COMPUTER = {enterprise.LibraryComputer:N2}"
    ]

let drawEnterprise() =
    [   
        ""
        ""
        "                                    ,------*------,"
        "                    ,-------------   '---  ------'"
        "                     '-------- --'      / /"
        "                         ,---' '-------/ /--,"
        "                          '----------------'"
        "                    THE USS ENTERPRISE --- NCC-1701"
        ""
        ""
    ]
