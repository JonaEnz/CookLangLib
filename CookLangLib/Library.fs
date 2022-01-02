namespace CookLangLib

open System.Text.Json
open UnitsNet.Units
open CookLangLib.RecipeExtensions

module Main =

    let fromFile config path = Recipe.fromFile config path
