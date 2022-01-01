namespace CookLangLib

open System.IO

module Helper =
    let configFromFile path =
        File.ReadAllText path |> Parser.parseConfig
