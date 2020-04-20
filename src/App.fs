module App

open Elmish
open Elmish.React
open Feliz
open Model
open System

type Page =
    | Intro
    | Scenario of int
    | End

type State = {
    Count: int
    User: User
    Scenarios: RecommendationScenario list
    LikedRecipeIds: Set<RecipeId>
    CurrentPage: Page
    StartUtc: DateTime
}

type Msg =
    | NameChanged of string
    | Start
    | ToggleRecipe of RecipeId
    | FinishScenario of int

let init() =
    {
        Count = 0
        User = {
            Name = "anonymous"
        }
        Scenarios = loadFrom "data.json"
        LikedRecipeIds = Set.empty
        CurrentPage = Intro
        StartUtc = DateTime.UtcNow
    }

let update (msg: Msg) (state: State): State =
    match msg with
    | NameChanged name ->
        { state with User = { state.User with Name = name } }
    | Start ->
        if List.isEmpty state.Scenarios
            then { state with CurrentPage = End }
            else { state with CurrentPage = (Scenario 0) }
    | FinishScenario index ->
        if List.length state.Scenarios >= index
            then { state with CurrentPage = End }
            else { state with CurrentPage = (Scenario <| index + 1) }
    | ToggleRecipe _ ->
        { state with Count = state.Count - 3 }

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

let scenarioPage dispatch index scenario (userName: string) =
    Html.div [
        Html.h1 userName
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
        scenarioPage dispatch index scenario state.User.Name
    | End -> lastPage

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run