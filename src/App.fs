module App

open Browser
open Elmish
open Elmish.React
open Model
open System
open FSharp.Core
open Feliz
open Fetch

type Page =
    | Intro
    | Scenario of int
    | End

type MethodIndex = {
    MethodId: RecommendationMethodId
    ScenarionIndex: int
}

type State = {
    User: User
    Scenarios: RecommendationScenario array
    SelectedRecipeIds: Set<RecipeId>
    MethodRatings: Map<MethodIndex, MethodOpinion>
    CurrentPage: Page
    StartUtc: DateTime
}

type Msg =
    | LoadData
    | DataLoaded of RecommendationScenario array
    | NameChanged of string
    | Start
    | ToggleRecipe of RecipeId
    | RateMethod of MethodIndex * MethodOpinion
    | FinishScenario of int

let loadData () =
    fetch "data.json" [] |> Promise.bind (fun r -> r.json<RecommendationScenario array>())

let init =
    {
        User = {
            SessionId = Guid.NewGuid()
            Name = "anonymous"
        }
        Scenarios = [||]
        SelectedRecipeIds = Set.empty
        MethodRatings = Map.empty
        CurrentPage = Intro
        StartUtc = DateTime.UtcNow
    }

let randomize sqn =
    let rnd = Random()
    let rec scramble (sqn : seq<'T>) =
        let remove n sqn = sqn |> Seq.except [n]
        seq {
            let x = sqn |> Seq.item (rnd.Next(0, sqn |> Seq.length))
            yield x
            let sqn' = remove x sqn
            if not (sqn' |> Seq.isEmpty) then
                yield! scramble sqn'
        }
    scramble sqn

let update (msg: Msg) (state: State) =
    match msg with
    | LoadData ->
        (state, Cmd.OfPromise.either loadData () DataLoaded (fun error -> DataLoaded [||]))
    | DataLoaded scenarios ->
        let randomizedScenarios = scenarios |> Array.map (fun s -> { s with Recommendations = randomize s.Recommendations |> Seq.toArray })
        ({ state with Scenarios = randomizedScenarios }, Cmd.none)
    | NameChanged name ->
        ({ state with User = { state.User with Name = name } }, Cmd.none)
    | Start ->
        if Array.isEmpty state.Scenarios
            then ({ state with CurrentPage = End }, Cmd.none)
            else ({ state with CurrentPage = (Scenario 0) }, Cmd.none)
    | FinishScenario index ->
        Dom.window.scrollTo (0.0, 0.0)
        if (Array.length state.Scenarios) <= index + 1
            then ({ state with CurrentPage = End }, Cmd.none)
            else ({ state with CurrentPage = (Scenario <| index + 1) }, Cmd.none)
    | ToggleRecipe recipeId ->
        let newSelectedRecipeIds =
            if Set.contains recipeId state.SelectedRecipeIds
                then Set.remove recipeId state.SelectedRecipeIds
                else Set.add recipeId state.SelectedRecipeIds

        ({ state with SelectedRecipeIds = newSelectedRecipeIds }, Cmd.none)
    | RateMethod (index, opinion) ->
        ({ state with MethodRatings = Map.add index opinion state.MethodRatings }, Cmd.none)

module Html =
    let styledDiv (className: string) (children: ReactElement seq) =
        Html.div [
            prop.className className
            prop.children children
        ]

let initPage dispatch =
    Html.div [
        prop.className "container"
        prop.style [
            style.paddingTop (length.perc 10)
        ]
        prop.children [
            Html.styledDiv "row" [
                Html.h1 "SmartRecipes Survey"
                Html.p "Thanks for filling out the form ! It consists of few scenarios, where each is described at the top alongside with ingredient list in your shopping basket. Please evaluate both recipes by clicking on them and whole methods. Thanks ! :)"
            ]
            Html.styledDiv "row" [
                Html.label [
                    prop.text "Your name:"
                    prop.style [
                        style.marginRight 15
                    ]
                ]
                Html.input [
                    prop.type'.text
                    prop.onTextChange (fun s -> dispatch <| NameChanged s)
                ]
            ]
            Html.styledDiv "row" [
                Html.button [
                    prop.onClick (fun _ -> dispatch Start)
                    prop.className "btn btn-primary"
                    prop.text "Start"
                ]
            ]
        ]
    ]

let renderIngredient ingredient =
    Html.div [
        if ingredient.IsInputMatch
            then Html.strong ingredient.DisplayLine
            else Html.text ingredient.DisplayLine
    ]

let renderRecipe dispatch (recipe: Recipe) isSelected =
    let (cardClass, headerClass) =
        if isSelected
            then "card border-success", "card-header text-white bg-success"
            else "card", "card-header"

    Html.div [
        prop.className cardClass
        prop.onClick (fun _ -> dispatch <| ToggleRecipe recipe.Id)
        prop.style [
            style.cursor "pointer"
            style.margin (25, 0)
        ]
        prop.children [
            Html.div [
                prop.className headerClass
                prop.text recipe.Name
            ]
            Html.img [
                prop.src recipe.ImageUri
                prop.className "card-img"
                prop.alt recipe.Name
            ]
            Html.div [
                prop.className "card-body"
                prop.children [
                    for i in recipe.Ingredients do
                        renderIngredient i

                    Html.div [
                        prop.style [
                            style.marginTop 25
                        ]
                        prop.children [
                            Html.a [
                                prop.href recipe.Uri
                                prop.target "blank"
                                prop.onClick (fun e -> e.stopPropagation ())
                                prop.text "See whole recipe"
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let methodAliases = Map.ofList [
    ("f2v-256-10-tf-idf-cal", "Alpha")
    ("f2v-256-10-tf-idf-mmr", "Beta")
    ("tf-idf-cal", "Gamma")
    ("tf-idf-mmr", "Delta")
    ("tf-idf", "Epsilon")
    ("f2v-256-10", "Zeta")
]

let renderMethod dispatch (method: RecommendationMethod) selectedRecipeIds =
    Html.styledDiv "col-2" [
        Html.h3 (Map.find method.Id methodAliases)
        for recipe in method.Recommendations do
            renderRecipe dispatch recipe (Set.contains recipe.Id selectedRecipeIds)
    ]

let allowedMethodIds = [
    "f2v-256-10-tf-idf-cal";
    "f2v-256-10-tf-idf-mmr";
    "tf-idf-cal";
    "tf-idf-mmr";
    "tf-idf";
    "f2v-256-10";
]

let showMethod (method: RecommendationMethod) =
    List.contains method.Id allowedMethodIds

let scenarioPage dispatch index scenario selectedRecipeIds =
    Html.div [
        Html.styledDiv "container-fluid" [
            Html.h3 "Scenario:"
            Html.paragraph scenario.Description
            Html.h3 "Your basket contains:"
            Html.unorderedList (scenario.Input |> Array.map Html.listItem)

            Html.styledDiv "row" [
                for method in scenario.Recommendations do
                    if showMethod method then
                        renderMethod dispatch method selectedRecipeIds
            ]
        ]
        Html.footer [
            prop.className "footer bg-light text-center"
            prop.style [
                style.position.fixedRelativeToWindow
                style.bottom 0
                style.left 0
                style.right 0
                style.height 48
                style.padding 5
            ]
            prop.children [
                Html.button [
                    prop.onClick (fun _ -> dispatch <| FinishScenario index)
                    prop.className "btn btn-primary"
                    prop.text "Next scenario"
                ]
            ]
        ]
    ]

let lastPage =
    Html.h1 "Thank you for your time!"

let render (state: State) (dispatch: Msg -> unit) =
    match state.CurrentPage with
    | Intro -> initPage dispatch
    | Scenario index ->
        let scenario = state.Scenarios.[index]
        scenarioPage dispatch index scenario state.SelectedRecipeIds
    | End -> lastPage

Program.mkProgram (fun _ -> (init, Cmd.ofMsg LoadData)) update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run