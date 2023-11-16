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
    }

val createEnterprise: int * int -> Enterprise

val copyEnterprise: enterprise: Enterprise -> Enterprise