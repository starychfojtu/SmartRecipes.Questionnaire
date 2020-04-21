module App

open Browser
open Elmish
open Elmish.React
open Model
open System
open System
open System
open FSharp.Core
open Feliz
open Fetch

type ScenarioIndex = int

type Page =
    | Intro
    | Scenario of ScenarioIndex
    | End

type MethodIndex = {
    MethodId: RecommendationMethodId
    ScenarioIndex: ScenarioIndex
}

type State = {
    User: User
    Scenarios: RecommendationScenario array
    SelectedRecipeIds: Map<ScenarioIndex, Set<RecipeId>>
    MethodRatings: Map<MethodIndex, MethodOpinion>
    CurrentPage: Page
    StartUtc: DateTime
}

type Msg =
    | LoadData
    | DataLoaded of RecommendationScenario array
    | NameChanged of string
    | Start
    | ToggleRecipe of ScenarioIndex * RecipeId
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
        SelectedRecipeIds = Map.empty
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
        let emptySelectedRecipes = scenarios |> Array.mapi (fun index _ -> (index, Set.empty)) |> Map.ofArray
        ({ state with Scenarios = randomizedScenarios; SelectedRecipeIds = emptySelectedRecipes }, Cmd.none)
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
    | ToggleRecipe (index, recipeId) ->
        let selectedRecipeIds = Map.find index state.SelectedRecipeIds
        let newSelectedRecipeIds =
            if Set.contains recipeId selectedRecipeIds
                then Set.remove recipeId selectedRecipeIds
                else Set.add recipeId selectedRecipeIds

        ({ state with SelectedRecipeIds = Map.add index newSelectedRecipeIds state.SelectedRecipeIds }, Cmd.none)
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

let renderRecipe dispatch index (recipe: Recipe) isSelected =
    let (cardClass, headerClass, bodyClass, anchorClass) =
        if isSelected
            then "card border-success", "card-header text-white bg-success", "card-body text-white bg-success", "text-white"
            else "card", "card-header", "card-body", ""

    Html.div [
        prop.className cardClass
        prop.onClick (fun _ -> dispatch <| ToggleRecipe (index, recipe.Id))
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
                prop.className bodyClass
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
                                prop.className anchorClass
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

let renderMethod dispatch index (method: RecommendationMethod) selectedRecipeIds =
    Html.styledDiv "col-2" [
        Html.h3 (Map.find method.Id methodAliases)
        for recipe in method.Recommendations do
            renderRecipe dispatch index recipe (Set.contains recipe.Id selectedRecipeIds)
    ]

let renderMethodRating dispatch methodIndex (method: RecommendationMethod) (currentOpinion: MethodOpinion option) recipesLiked =
    let nameClassName =
        match currentOpinion with
        | Some Great -> "text-success"
        | Some Average -> "text-info"
        | Some Bad -> "text-danger"
        | _ -> ""

    Html.styledDiv "col-2" [
        Html.h3 [
            prop.className nameClassName
            prop.text (Map.find method.Id methodAliases)
        ]
        Html.p [
            prop.text (sprintf "%i Recipes liked" recipesLiked)
        ]
        Html.button [
            prop.className "btn btn-success"
            prop.disabled (match currentOpinion with Some Great -> true | _ -> false)
            prop.onClick (fun _ -> dispatch (RateMethod (methodIndex, Great)))
            prop.style [
                style.margin 4
            ]
            prop.text "Great"
        ]
        Html.button [
            prop.className "btn btn-info"
            prop.disabled (match currentOpinion with Some Average -> true | _ -> false)
            prop.onClick (fun _ -> dispatch (RateMethod (methodIndex, Average)))
            prop.style [
                style.margin 4
            ]
            prop.text "Ok"
        ]
        Html.button [
            prop.className "btn btn-danger"
            prop.disabled (match currentOpinion with Some Bad -> true | _ -> false)
            prop.onClick (fun _ -> dispatch (RateMethod (methodIndex, Bad)))
            prop.style [
                style.margin 4
            ]
            prop.text "Bad"
        ]
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

let scenarioPage dispatch index totalScenarios scenario selectedRecipeIds methodRatings =
    let methods = scenario.Recommendations |> Array.filter showMethod
    Html.div [
        Html.styledDiv "container-fluid" [
            Html.h3 (sprintf "Scenario (%i/%i):" (index + 1) totalScenarios)
            Html.paragraph scenario.Description
            Html.h3 "Your basket contains:"
            Html.unorderedList (scenario.Input |> Array.map Html.listItem)

            Html.styledDiv "row" [
                for method in methods do
                    renderMethod dispatch index method selectedRecipeIds
            ]
        ]
        Html.footer [
            prop.className "footer bg-light text-center"
            prop.style [
                style.position.fixedRelativeToWindow
                style.bottom 0
                style.left 0
                style.right 0
                style.padding 5
            ]
            prop.children [
                Html.styledDiv "row" [
                    for method in methods do
                        let methodIndex = {
                            ScenarioIndex = index
                            MethodId = method.Id
                        }
                        let methodRecipes = method.Recommendations |> Array.map (fun r -> r.Id) |> Set.ofArray
                        let recipesLiked = Set.intersect selectedRecipeIds methodRecipes |> Set.count
                        renderMethodRating dispatch methodIndex method (Map.tryFind methodIndex methodRatings) recipesLiked
                ]
                Html.button [
                    prop.onClick (fun _ -> dispatch <| FinishScenario index)
                    prop.className "btn btn-primary"
                    prop.text "Next scenario"
                ]
            ]
        ]
    ]

let lastPage =
    Html.div [
        prop.className "container"
        prop.style [
            style.paddingTop (length.perc 10)
        ]
        prop.children [
            Html.styledDiv "row" [
                Html.h1 "That it is ! Thank you for your time!"
            ]
        ]
    ]


let render (state: State) (dispatch: Msg -> unit) =
    match state.CurrentPage with
    | Intro -> initPage dispatch
    | Scenario index ->
        let scenario = state.Scenarios.[index]
        scenarioPage dispatch index (Array.length state.Scenarios) scenario (Map.find index state.SelectedRecipeIds) state.MethodRatings
    | End -> lastPage

Program.mkProgram (fun _ -> (init, Cmd.ofMsg LoadData)) update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run