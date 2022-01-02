namespace CookLangLib

open CookLangLib

module RecipeExtensions =
    type Recipe with
        member this.ingredients =
            let addIngs (m: Map<string, float>) (i: Ingredient) =
                if Map.containsKey i.name m |> not then
                    Map.add (i.name) (i.amount) m
                else
                    Map.change (i.name) (Option.map ((+) i.amount)) m

            let ingredients =
                List.collect (fun s -> s.ingredients) this.steps

            List.fold addIngs Map.empty ingredients
            |> Map.map (fun name amount ->
                { name = name
                  amount = amount
                  unit =
                    (List.find (fun (i: Ingredient) -> i.name = name) ingredients)
                        .unit })
            |> Map.values

        member this.cookware =
            List.collect (fun s -> s.cookware) this.steps

        member this.writeToFile config (fileName: string) =
            let recipe = this
            let recipeText = Writer.writeRecipe config recipe
            let file = System.IO.File.OpenWrite(fileName)
            let writer = new System.IO.StreamWriter(file)
            writer.Write(recipeText)
            writer.Close()

        member this.getServings =
            if this.metadata.ContainsKey "servings" then
                this.metadata.["servings"] |> int
            else
                1

        member this.setServings(i: int) =
            this.metadata.["servings"] = i.ToString()

        member this.getSource =
            if this.metadata.ContainsKey "source" then
                this.metadata.["source"]
            else
                ""

        member this.setSource(source: string) = this.metadata.["source"] = source

        static member fromFile config (filePath: string) =
            Parser.parseRecipe
                config
                (System.IO.File.ReadAllLines filePath
                 |> Array.toList)
