module App

open Elmish
open Elmish.React
open Feliz
open Model
open System

type State = {
    Count: int
    User: User
    Scenarios: RecommendationScenario list
    LikedRecipeIds: Set<RecipeId>
    CurrentScenarioIndex: int option
    StartUtc: DateTime
}

type Msg =
    | NameChanged of string
    | Start
    | ToggleRecipe of RecipeId

let init() =
    {
        Count = 0
        User = {
            Name = "anonymous"
        }
        Scenarios = loadFrom "data.json"
        LikedRecipeIds = Set.empty
        CurrentScenarioIndex = None
        StartUtc = DateTime.UtcNow
    }

let update (msg: Msg) (state: State): State =
    match msg with
    | NameChanged name ->
        { state with User = { state.User with Name = name } }
    | Start ->
        { state with CurrentScenarioIndex = (Some 0) }
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

let render (state: State) (dispatch: Msg -> unit) =
    match state.CurrentScenarioIndex with
    | Some index -> Html.h1 state.User.Name
    | None -> initPage dispatch

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run