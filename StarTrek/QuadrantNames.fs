module QuadrantNames

open Domain

let quadrantName quadrant =
        let partNames = [" I"; " II"; " III"; " IV";]

        if (snd quadrant) < 4 then
            match fst quadrant with
            | 0 -> "ANTARES" + partNames.[snd quadrant % 4]
            | 1 -> "RIGEL" + partNames.[snd quadrant % 4]
            | 2 -> "PROCYON" + partNames.[snd quadrant % 4]
            | 3 -> "VEGA" + partNames.[snd quadrant % 4]
            | 4 -> "CANOPUS" + partNames.[snd quadrant % 4]
            | 5 -> "ALTAIR" + partNames.[snd quadrant % 4]
            | 6 -> "SAGITTARIUS" + partNames.[snd quadrant % 4]
            | 7 -> "POLLUX" + partNames.[snd quadrant % 4]
            | _ -> ""
        else
            match fst quadrant with
            | 0 -> "SIRIUS" + partNames.[snd quadrant % 4]
            | 1 -> "DENEB" + partNames.[snd quadrant % 4]
            | 2 -> "CAPELLA" + partNames.[snd quadrant % 4]
            | 3 -> "BETELGEUSE" + partNames.[snd quadrant % 4]
            | 4 -> "ALDEBARAN" + partNames.[snd quadrant % 4]
            | 5 -> "REGULUS" + partNames.[snd quadrant % 4]
            | 6 -> "ARCTURUS" + partNames.[snd quadrant % 4]
            | 7 -> "SPICA" + partNames.[snd quadrant % 4]
            | _ -> ""

let quadrantNameAlt quadrant =
        if (snd quadrant) < 4 then
            match fst quadrant with
            | 0 -> "ANTARES    "
            | 1 -> "RIGEL      "
            | 2 -> "PROCYON    "
            | 3 -> "VEGA       "
            | 4 -> "CANOPUS    "
            | 5 -> "ALTAIR     "
            | 6 -> "SAGITTARIUS"
            | 7 -> "POLLUX     "
            | _ -> ""
        else
            match fst quadrant with
            | 0 -> "SIRIUS     "
            | 1 -> "DENEB      "
            | 2 -> "CAPELLA    "
            | 3 -> "BETELGEUSE "
            | 4 -> "ALDEBARAN  "
            | 5 -> "REGULUS    "
            | 6 -> "ARCTURUS   "
            | 7 -> "SPICA      "
            | _ -> ""
