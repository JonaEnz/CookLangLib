namespace CookLangLib

open UnitsNet
open System.Text.RegularExpressions

module Writer =
    let getUnitName config amount unit =
        let getMassName config (mass: Mass) =
            let converted =
                config.weightUnits
                |> List.map (fun (u, s) -> (mass.ToUnit u), s)

            List.filter (fun (m: Mass, s) -> m.Value > 1.0) converted
            |> List.sortBy (fun (m, _) -> m.Value)
            |> List.tryHead
            |> Option.defaultValue (List.head converted)
            |> fun (m, s) -> m.Value.ToString() + "%" + s

        let getVolumeName config (vol: Volume) =
            let converted =
                config.volumeUnits
                |> List.map (fun (u, s) -> vol.ToUnit u, s)

            List.filter (fun (v: Volume, s) -> v.Value > 1.0) converted
            |> List.sortBy (fun (v, _) -> v.Value)
            |> List.tryHead
            |> Option.defaultValue (List.head converted)
            |> fun (v, s) -> v.Value.ToString() + "%" + s

        match unit with
        | Mass m -> getMassName config (Mass(amount, m))
        | Volume v -> getVolumeName config (Volume(amount, v))
        | Amount -> ""

    let writeIngredient config (ing: Ingredient) =
        if ing.amount <> 1.0 then
            if (ing.unit <> Amount) then
                $"@{ing.name} "
                + "{"
                + getUnitName config ing.amount ing.unit
                + "}"
            else
                $"@{ing.name} "
                + "{"
                + $"{ing.amount.ToString()}"
                + "}"
        else
            $"@{ing.name}" + "{}"

    let writeCookware (c: Cookware) = "#" + c.name + "{}"

    let writeTimer (t: Timer) =
        "#"
        + t.name
        + "{"
        + t.length.TotalSeconds.ToString()
        + "%"
        + "seconds"
        + "}"

    let writeStep config (s: Step) =
        let innerPrint text =
            match Regex.Match(text, "\$I(\d+)"), Regex.Match(text, "\$C(\d+)"), Regex.Match(text, "\$T(\d+)") with
            | m, _, _ when m.Success ->
                let i = (m.Groups[1]).Value |> int

                Regex.Replace(text, "\$I(\d+)", writeIngredient config (s.ingredients[i]))
                |> Some
            | m, _, _ when m.Success ->
                let i = int (m.Groups[1].Value)

                Regex.Replace(text, "\$C(\d+)", writeCookware (s.cookware[ i ]))
                |> Some
            | m, _, _ when m.Success ->
                let i = int (m.Groups[1].Value)

                Regex.Replace(text, "\$T(\d+)", writeTimer (s.timers[i]))
                |> Some
            | _ -> None

        List.unfold (fun (state: string) -> state |> innerPrint |> Option.map (fun s -> s, s)) s.text
        |> List.last
        |> fun str -> 
            if s.comment.Length > 0 
            then str + "--" + s.comment 
            else str

    let writeRecipe config (r: Recipe) =
        let steps = List.map (writeStep config) r.steps
        //let metadata = List.map (fun (k,v) -> $">> {k}: {v}") (Map.toList r.metadata)
        System.String.Join('\n', steps)
