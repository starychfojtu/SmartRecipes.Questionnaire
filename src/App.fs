module App

open Elmish
open Elmish.React
open Model
open System
open System
open FSharp.Core
open Fable.Core
open Fable.Core
open Feliz
open Fetch

type Page =
    | Intro
    | Scenario of int
    | End

type State = {
    User: User
    Scenarios: RecommendationScenario array
    LikedRecipeIds: Set<RecipeId>
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
        LikedRecipeIds = Set.empty
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
    | ToggleRecipe _ ->
        (state, Cmd.none)

let initPage dispatch =
    Html.div [
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
    Html.listItem ingredient.DisplayLine

let renderRecipe (recipe: Recipe) =
    Html.div [
        Html.anchor [
            prop.href recipe.Uri
            prop.children [
                Html.h4 recipe.Name
            ]
        ]
        Html.unorderedList (recipe.Ingredients |> Array.map renderIngredient)
    ]

let renderMethod (method: RecommendationMethod) =
    Html.div [
        Html.h3 method.Name
        for recipe in method.Recommendations do
            renderRecipe recipe
    ]

let scenarioPage dispatch index scenario =
    Html.div [
        Html.paragraph scenario.Description
        Html.h3 "Your basket contains:"
        Html.unorderedList (scenario.Input |> Array.map Html.listItem)

        for method in scenario.Recommendations do
            renderMethod method

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
        scenarioPage dispatch index scenario
    | End -> lastPage

Program.mkProgram (fun _ -> (init, Cmd.ofMsg LoadData)) update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run