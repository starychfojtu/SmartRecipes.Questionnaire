module App

open Elmish
open Elmish.React
open Model
open System
open System
open System
open FSharp.Core
open Fable.Core
open Fable.Core
open Fable.React.Props
open Feliz
open Fetch

type Page =
    | Intro
    | Scenario of int
    | End

type State = {
    User: User
    Scenarios: RecommendationScenario array
    SelectedRecipeIds: Set<RecipeId>
    CurrentPage: Page
    StartUtc: DateTime
}

type Msg =
    | LoadData
    | DataLoaded of RecommendationScenario array
    | NameChanged of string
    | Start
    | ToggleRecipe of RecipeId
    | FinishScenario of int

let loadData () =
    fetch "data.json" [] |> Promise.bind (fun r -> r.json<RecommendationScenario array>())

let init =
    {
        User = {
            Name = "anonymous"
        }
        Scenarios = [||]
        SelectedRecipeIds = Set.empty
        CurrentPage = Intro
        StartUtc = DateTime.UtcNow
    }

let update (msg: Msg) (state: State) =
    match msg with
    | LoadData ->
        (state, Cmd.OfPromise.either loadData () DataLoaded (fun error -> DataLoaded [||]))
    | DataLoaded scenarios ->
        ({ state with Scenarios = scenarios }, Cmd.none)
    | NameChanged name ->
        ({ state with User = { state.User with Name = name } }, Cmd.none)
    | Start ->
        if Array.isEmpty state.Scenarios
            then ({ state with CurrentPage = End }, Cmd.none)
            else ({ state with CurrentPage = (Scenario 0) }, Cmd.none)
    | FinishScenario index ->
        if Array.length state.Scenarios >= index
            then ({ state with CurrentPage = End }, Cmd.none)
            else ({ state with CurrentPage = (Scenario <| index + 1) }, Cmd.none)
    | ToggleRecipe recipeId ->
        let newSelectedRecipeIds =
            if Set.contains recipeId state.SelectedRecipeIds
                then Set.remove recipeId state.SelectedRecipeIds
                else Set.add recipeId state.SelectedRecipeIds

        ({ state with SelectedRecipeIds = newSelectedRecipeIds }, Cmd.none)

module Html =
    let styledDiv (className: string) (children: ReactElement seq) =
        Html.div [
            prop.className className
            prop.children children
        ]

let initPage dispatch =
    Html.styledDiv "container" [
        Html.h1 "Recommendation questionnaire"
        Html.input [
            prop.type'.text
            prop.onTextChange (fun s -> dispatch <| NameChanged s)
        ]
        Html.button [
            prop.onClick (fun _ -> dispatch Start)
            prop.text "Start"
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

    let imageUrl = "https://images.media-allrecipes.com/userphotos/560x315/225773.jpg"

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
                prop.src imageUrl
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
    Html.styledDiv "container-fluid" [
        Html.h3 "Scenario:"
        Html.paragraph scenario.Description
        Html.h3 "Your basket contains:"
        Html.unorderedList (scenario.Input |> Array.map Html.listItem)

        Html.styledDiv "row" [
            // TODO: randomize order.
            for method in scenario.Recommendations do
                if showMethod method then
                    renderMethod dispatch method selectedRecipeIds
        ]

        Html.button [
            prop.onClick (fun _ -> dispatch <| FinishScenario index)
            prop.text "Next"
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