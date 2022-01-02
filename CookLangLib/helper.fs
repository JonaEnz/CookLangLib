namespace CookLangLib

open System.IO

module Helper =
    let configFromFile path =
        File.ReadAllText path |> Parser.parseConfig

    let inline (+) c1 c2 =
        { c1 with
            weightUnits = List.append c1.weightUnits c2.weightUnits
            volumeUnits = List.append c1.volumeUnits c2.volumeUnits }
