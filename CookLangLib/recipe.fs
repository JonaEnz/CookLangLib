namespace CookLangLib

module Recipe =

    let getIngredients recipe =
        let addIngs (m: Map<string, float>) (i: Ingredient) =
            if Map.containsKey i.name m |> not then
                Map.add (i.name) (i.amount) m
            else
                Map.change (i.name) (Option.map ((+) i.amount)) m

        let ingredients =
            List.collect (fun s -> s.ingredients) recipe.steps

        List.fold addIngs Map.empty ingredients
        |> Map.map (fun name amount ->
            { name = name
              amount = amount
              unit =
                (List.find (fun (i: Ingredient) -> i.name = name) ingredients)
                    .unit })
        |> Map.values

    let getCookware recipe =
        List.collect (fun s -> s.cookware) recipe.steps

    let parse input = { steps = []; metadata = Map.empty }
