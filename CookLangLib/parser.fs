namespace CookLangLib

open System.Text.RegularExpressions
open System

module Parser =
    let parseUnit unit config =
        let parseConfList unit configList =
            List.tryFind (fun (_, s) -> s = unit) configList
            |> Option.map fst

        match (parseConfList unit config.weightUnits), (parseConfList unit config.volumeUnits) with
        | Some w, _ -> Unit.Mass w
        | _, Some v -> Unit.Volume v
        | None, None -> Amount

    let parseIngredient line config =

        let reg =
            Regex "@(?:([\w ]+){([\w \.]*)(?:%(\S+))?})|@(\w+)"

        let m = reg.Match line

        match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
               |> Seq.toList)
            with
        | [ name; amount; unit; _ ] when name <> "" && amount <> "" && unit <> "" ->
            { name = name
              unit = parseUnit unit config
              amount = float (amount) }
        | [ name; amount; _; _ ] when name <> "" && amount <> "" ->
            { name = name
              unit = Amount
              amount = float (amount) }
        | [ name; _; _; _ ] when name <> "" ->
            { name = name
              unit = Amount
              amount = 1.0 }
        | [ _; _; _; name ] when name <> "" ->
            { name = name
              unit = Amount
              amount = 1.0 }
        | _ -> failwith ("Could not parse ingredient: " + line)

    let parseCookware line =
        let reg = Regex "#([\w ]*){}|#(\w+)"
        let m = reg.Match line

        match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
               |> Seq.toList)
            with
        | [ name; _ ] when name <> "" -> { name = name; amount = 1 }
        | [ _; name ] when name <> "" -> { name = name; amount = 1 }
        | _ -> failwith ("Could not parse cookware: " + line)

    let parseTimer line = { name = ""; length = TimeSpan.Zero }
