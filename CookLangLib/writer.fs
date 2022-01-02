namespace CookLangLib

open UnitsNet
open System.Text.RegularExpressions
open System.Globalization

module Writer =
    let getMassName config (mass: Mass) =
        let converted =
            config.weightUnits
            |> List.map (fun (u, s) -> (mass.ToUnit u), s)

        List.filter (fun (m: Mass, _) -> m.Value >= 1.0) converted
        |> List.sortBy (fun (m, _) -> m.Value)
        |> List.tryHead
        |> Option.defaultValue (List.head converted)
        |> fun (m, s) ->
            (System.Math.Round(m.Value, 2))
                .ToString(CultureInfo.InvariantCulture)
            + "%"
            + s

    let getVolumeName config (vol: Volume) =
        let converted =
            config.volumeUnits
            |> List.map (fun (u, s) -> vol.ToUnit u, s)

        List.filter (fun (v: Volume, s) -> v.Value >= 1.0) converted
        |> List.sortBy (fun (v, _) -> v.Value)
        |> List.tryHead
        |> Option.defaultValue (List.head converted)
        |> fun (v, s) ->
            (System.Math.Round(v.Value, 2))
                .ToString(CultureInfo.InvariantCulture)
            + "%"
            + s


    let getUnitName config amount unit =

        match unit with
        | Mass m -> getMassName config (Mass(amount, m))
        | Volume v -> getVolumeName config (Volume(amount, v))
        | Amount -> ""

    let writeIngredient config (ing: Ingredient) =
        match ing.amount <> 1.0, ing.unit with
        | (true, Amount) ->
            $"@{ing.name} "
            + "{"
            + $"{ing.amount.ToString()}"
            + "}"
        | (false, Amount) -> $"@{ing.name}" + "{}"
        | (_, u) ->
            $"@{ing.name} "
            + "{"
            + getUnitName config ing.amount u
            + "}"

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
                let i = (m.Groups.[1]).Value |> int

                (Regex "\$I(\d+)")
                    .Replace(text, writeIngredient config (s.ingredients.[i]), 1)
                |> Some
            | _, m, _ when m.Success ->
                let i = int (m.Groups.[1].Value)

                (Regex "\$C(\d+)")
                    .Replace(text, writeCookware (s.cookware.[i]), 1)
                |> Some
            | _, _, m when m.Success ->
                let i = int (m.Groups.[1].Value)

                (Regex "\$T(\d+)")
                    .Replace(text, writeTimer (s.timers.[i]), 1)
                |> Some
            | _ -> None

        List.unfold (fun (state: string) -> state |> innerPrint |> Option.map (fun s -> s, s)) s.text
        |> List.tryLast
        |> Option.defaultValue s.text
        |> fun str ->
            if s.comment.Length > 0 then
                str + "--" + s.comment
            else
                str

    let writeRecipe config (r: Recipe) =
        let steps = List.map (writeStep config) r.steps

        let metadata =
            List.map (fun (k, v) -> $">> {k}: {v}") (Map.toList r.metadata)

        System.String.Join('\n', List.append metadata steps)
