namespace CookLangLib

type Step() =
    let text: string = ""
    let ingredients: Ingredient list = []
    let cookware: Cookware list = []
    let timers: Timer list = []
    member this.setText = fun text -> this.text = text
