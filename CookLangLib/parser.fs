namespace CookLangLib

open System
open System.Text.Json
open System.Text.RegularExpressions
module Parser =
    let addIndexTo (s:string) (line:string) =
        let addIndexToInner (s:string) (line:string) (i:int) =
            let r = Regex ("(" + Regex.Escape s + ")($|[^\d])")
            let m = r.Match line
            if m.Success && i < 10 then
                (r.Replace(line, s + i.ToString() + "$2", 1), i+1) |> Some
            else
                None
        List.unfold (fun state -> 
            state 
            ||> addIndexToInner s
            |> Option.map (fun (s,i) -> s,(s,i))) 
            (line,0)
        |> fun l -> if List.length l > 0 then List.last l else line

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
            Regex @"@(?:([\w ]+){([\w \.]*)(?:%(\S+))?})|@(\w+)"

        let m = reg.Match line

        if not m.Success then None else

        let out = reg.Replace(line, "$I", 1)

        
        let res = 
            match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
               |> Seq.toList)
            with
            | [ _;name; amount; unit; _ ] when name <> "" && amount <> "" && unit <> "" ->
                { name = name;
                unit = parseUnit unit config;
                amount = float (amount) },
                out
            | [ _;name; amount; _; _ ] when name <> "" && amount <> "" ->
                { name = name;
                unit = Amount;
                amount = float (amount) },
                out
            | [ _;name; _; _; _ ] when name <> "" ->
                { name = name;
                unit = Amount;
                amount = 1.0 },
                out
            | [ _;_; _; _; name ] when name <> "" ->
                { name = name;
                unit = Amount;
                amount = 1.0 },
                out
            | _ -> failwith ("Could not parse ingredient: " + line)
        Some res

    let parseCookware line =
        let reg = Regex @"#([\w ]*){}|#(\w+)"
        let m = reg.Match line
        if not m.Success then None else

        let out = reg.Replace(line, "$C", 1)

        let res =
            match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
                |> Seq.toList)
                with
            | [_; name; _ ] when name <> "" -> { name = name; amount = 1 }, out
            | [_; _; name ] when name <> "" -> { name = name; amount = 1 }, out
            | _ -> failwith ("Could not parse cookware: " + line)
        Some res

    let parseTimer (line: string) =
        let reg =
            Regex @"~(.*){(\d+(?:\.\d+)?)%(hour|minute|second)s?}"

        let m = reg.Match(line)
        if not m.Success then None else

        let out=
            reg.Replace(line, "$T", 1)

        match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
               |> Seq.toList)
            with
        | [ _; name; length; "hour" ] ->
            ({ name = name
               length = TimeSpan.FromHours(float (length)) },
             out)
            |> Some
        | [ _; name; length; "minute" ] ->
            ({ name = name
               length = TimeSpan.FromMinutes(float (length)) },
             out)
            |> Some
        | [ _; name; length; _ ] ->
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
            let commentReg = Regex @"(.*)\[-(.*)-\](.*)"
            if line.Split("--").Length > 1 then
                (line.Split("--") |> Array.head, line.Split("--") |> Array.last)
            else

            let comment = commentReg.Match line
            if not comment.Success then line, "" else
            (comment.Groups[1].Value + " " + comment.Groups[3].Value, comment.Groups[2].Value)

        List.unfold 
            (fun (l,s) -> innerParse l s |> Option.map (fun (ste, str) -> (str,ste),(str,ste)))
            (line, ({comment=""; text="";ingredients = []; cookware = []; timers = [] }))
        |> List.last
        |> fun (str,ste) -> 
            {ste with 
                    comment = snd (splitComment str)
                    text = (splitComment str) 
                        |> fst 
                        |> addIndexTo "$I" 
                        |> addIndexTo "$C" 
                        |> addIndexTo "$T";
            }

    let parseRecipe config (recipe: string list) =
        let metadataReg = Regex @">> (.*): (.*)"
        let parseMetadata map line =
            let m = metadataReg.Match line
            if not m.Success then map else

            match ((Seq.map (fun (g: Group) -> g.Value) m.Groups.Values)
                |> Seq.toList)
            with
            | [ _; key; value ] when key <> "" -> Map.add key value map
            | _ -> failwith ("Could not parse metadata: " + line)
        
        
        List.fold (fun (metadata,steps) line -> 
        if metadataReg.IsMatch line then 
            (parseMetadata metadata line, steps)
        else
            (metadata, List.append steps [parseStep config line])
        ) (Map.empty, []) recipe
        |> fun (metadata, steps) ->{ metadata = metadata; steps = steps;}

    let parseConfig (json:string) =
        try 
            JsonSerializer.Deserialize<LanguageConfiguration>(json)
        with
        | _ -> failwith ("Could not parse config: " + json)