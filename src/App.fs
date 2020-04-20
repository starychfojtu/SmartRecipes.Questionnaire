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
    CurrentScenarioIndex: int
    StartUtc: DateTime
}

type Msg =
    | ToggleRecipe of RecipeId

let init() =
    {
        Count = 0
        User = {
            Name = "anonymous"
        }
        Scenarios = loadFrom "data.json"
        LikedRecipeIds = Set.empty
        CurrentScenarioIndex = 0
        StartUtc = DateTime.UtcNow
    }

let update (msg: Msg) (state: State): State =
    match msg with
    | ToggleRecipe _ ->
        { state with Count = state.Count - 3 }

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    Html.button [
      prop.onClick (fun _ -> dispatch (ToggleRecipe ""))
      prop.text "Increment"
    ]

    Html.button [
      prop.onClick (fun _ -> dispatch (ToggleRecipe ""))
      prop.text "Decrement"
    ]

    Html.h1 state.Count
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run