module App

open Elmish
open Elmish.React
open Feliz
open Model

type State = {
    Count: int
    User: User
    Scenarios: RecommendationScenario list
}

type Msg =
    | Increment
    | Decrement

let init() =
    {
        Count = 0
        User = {
            Name = "anonymous"
        }
        Scenarios = loadFrom "data.json"
    }

let update (msg: Msg) (state: State): State =
    match msg with
    | Increment ->
        { state with Count = state.Count + 2 }

    | Decrement ->
        { state with Count = state.Count - 2 }

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    Html.button [
      prop.onClick (fun _ -> dispatch Increment)
      prop.text "Increment"
    ]

    Html.button [
      prop.onClick (fun _ -> dispatch Decrement)
      prop.text "Decrement"
    ]

    Html.h1 state.Count
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run