module Enterprise

open Domain

type Enterprise =
    {
      SectorId: SectorId
      Condition: Condition
      Energy: int
      ShieldEnergy: int
      Torpedoes: int
      WarpEngines: double
      ShortRangeSensors: double
      LongRangeSensors: double
      PhaserControl: double
      PhotonTubes: double
      DamageControl: double
      ShieldControl: double
      LibraryComputer: double
      IsDocked: bool
      Symbol: string
    }

val createEnterprise: int * int -> Enterprise
val copyEnterprise: enterprise: Enterprise -> Enterprise
val getDevice: enterprise: Enterprise -> deviceNo: int -> double
val setDevice: enterprise: Enterprise -> deviceNo: int -> value: double -> Enterprise
val getDeviceName: deviceNo: int -> string
val getConditionStr: enterprise: Enterprise -> string
val getDamage: enterprise: Enterprise -> string list
val drawEnterprise: unit -> string list