namespace CookLangLib

open UnitsNet
open UnitsNet.Units
open System

type Unit =
    | Mass of MassUnit
    | Amount
    | Volume of VolumeUnit

type Ingredient =
    { name: string
      unit: Unit
      amount: float }

type Cookware = { name: string; amount: int }

type Timer = { name: string; length: TimeSpan }

type Step =
    { text: string
      ingredients: Ingredient list
      cookware: Cookware list
      timers: Timer list }

type Recipe =
    { steps: Step list
      metadata: Map<string, string> }

type LanguageConfiguration =
    { minute: string
      minutes: string
      second: string
      seconds: string
      hour: string
      hours: string
      weightUnits: (MassUnit * string) list
      volumeUnits: (VolumeUnit * string) list }
