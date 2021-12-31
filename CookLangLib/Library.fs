namespace CookLangLib

open System.Text.Json
open UnitsNet.Units

module Test =

    let test =
        let a: LanguageConfiguration =
            { minute = "minute"
              minutes = "minutes"
              second = "second"
              seconds = "seconds"
              hour = "hour"
              hours = "hours"
              weightUnits =
                [ (MassUnit.Gram, "g")
                  (MassUnit.Kilogram, "kg")
                  (MassUnit.Ounce, "oz")
                  (MassUnit.Pound, "lb") ]
              volumeUnits =
                [ (VolumeUnit.Milliliter, "ml")
                  (VolumeUnit.Liter, "l")
                  (VolumeUnit.UsOunce, "oz")
                  (VolumeUnit.UsPint, "pt")
                  (VolumeUnit.UsQuart, "qt")
                  (VolumeUnit.UsGallon, "gal") ] }

        JsonSerializer.Serialize a
