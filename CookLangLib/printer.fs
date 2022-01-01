namespace CookLangLib

open System.Text.RegularExpressions

module Printer =
    let printIngredient (ing: Ingredient) =
        $"{ing.amount} ({ing.unit.ToString()} {ing.name})"

    let printCookware (cw: Cookware) = $"{cw.amount} {cw.name}"
    let printTimer (t: Timer) = $"{t.name}: {t.length.ToString()}"

    let printStep (s: Step) =
        let innerPrint text =
            match Regex.Match(text, "\$I(\d+)"), Regex.Match(text, "\$C(\d+)"), Regex.Match(text, "\$T(\d+)") with
            | m, _, _ when m.Success ->
                let i = (m.Groups[ 1 ]).Value |> int
                Regex.Replace(text, "\$I(\d+)", printIngredient (s.ingredients[ i ])) |> Some
            | m, _, _ when m.Success ->
                let i = int (m.Groups[ 1 ].Value)
                Regex.Replace(text, "\$C(\d+)", printCookware (s.cookware[ i ])) |> Some
            | m, _, _ when m.Success ->
                let i = int (m.Groups[ 1 ].Value)
                Regex.Replace(text, "\$T(\d+)", printTimer (s.timers[ i ])) |> Some
            | _ -> None
        List.unfold (fun (state:string) -> 
            state 
            |> innerPrint
            |> Option.map (fun s -> s,s) 
        ) s.text
        |> List.last
    
    let printRecipe (r: Recipe) =
        let steps = List.map printStep r.steps
        //let metadata = List.map (fun (k,v) -> $">> {k}: {v}") (Map.toList r.metadata)
        System.String.Join('\n', steps)

