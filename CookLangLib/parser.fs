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

    let parseIngredient config line=

        let reg =
            Regex "@(?:([\w ]+){([\w \.]*)(?:%(\S+))?})|@(\w+)"

        let m = reg.Match line

        if not m.Success then None else

        let out = reg.Replace(line, "\$I", 1)

        
        let res = 
            match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
               |> Seq.toList)
            with
            | [ name; amount; unit; _ ] when name <> "" && amount <> "" && unit <> "" ->
                { name = name;
                unit = parseUnit unit config;
                amount = float (amount) },
                out
            | [ name; amount; _; _ ] when name <> "" && amount <> "" ->
                { name = name;
                unit = Amount;
                amount = float (amount) },
                out
            | [ name; _; _; _ ] when name <> "" ->
                { name = name;
                unit = Amount;
                amount = 1.0 },
                out
            | [ _; _; _; name ] when name <> "" ->
                { name = name;
                unit = Amount;
                amount = 1.0 },
                out
            | _ -> failwith ("Could not parse ingredient: " + line)
        Some res

    let parseCookware line =
        let reg = Regex "#([\w ]*){}|#(\w+)"
        let m = reg.Match line
        if not m.Success then None else

        let out = reg.Replace(line, "\$C", 1)

        let res =
            match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
                |> Seq.toList)
                with
            | [ name; _ ] when name <> "" -> { name = name; amount = 1 }, out
            | [ _; name ] when name <> "" -> { name = name; amount = 1 }, out
            | _ -> failwith ("Could not parse cookware: " + line)
        Some res

    let parseTimer (line: string) =
        let reg =
            Regex "~(.*){(\d+(?:\.\d+)?)%()}"

        let m = reg.Match(line.ToLower())
        if not m.Success then None else

        let out=
            reg.Replace(line.ToLower(), "\$T", 1)

        match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
               |> Seq.toList)
            with
        | [ name; length; "hour" ] ->
            ({ name = name
               length = TimeSpan.FromHours(float (length)) },
             out)
            |> Some
        | [ name; length; "minute" ] ->
            ({ name = name
               length = TimeSpan.FromMinutes(float (length)) },
             out)
            |> Some
        | [ name; length; _ ] ->
            ({ name = name
               length = TimeSpan.FromSeconds(float (length)) },
             out)
            |> Some
        | _ -> failwith ("Could not parse timer: " + line)

    let parseStep config line =
        let innerParse line step =
            match (parseIngredient config line), (parseCookware line), (parseTimer line) with
            | Some (i,s), _, _ -> Some ({step with ingredients = (List.append step.ingredients [i])}, s)
            | _, Some (c,s), _ -> Some ({step with cookware = (List.append step.cookware [c])}, s)
            | _, _, Some (t,s) -> Some ({step with timers = (List.append step.timers [t])}, s)
            | _, _, _ -> None
        let splitComment (line:string) =
            let commentReg = Regex "(.*)\[-(.*)-\](.*)"
            if line.Split("--").Length > 1 then
                (line.Split("--") |> Array.head, line.Split("--") |> Array.last)
            else

            let comment = commentReg.Match line
            if not comment.Success then line, "" else
            (comment.Captures[0].Value + " " + comment.Captures[2].Value, comment.Captures[1].Value)

        List.unfold 
            (fun (l,s) -> innerParse l s |> Option.map (fun (ste, str) -> (str,ste),(str,ste)))
            (line, ({comment=""; text="";ingredients = []; cookware = []; timers = [] }))
        |> List.last
        |> fun (str,ste) -> {ste with comment = snd (splitComment str); text = fst (splitComment str)}

