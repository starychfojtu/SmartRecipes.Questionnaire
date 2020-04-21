module App

open Browser
open Elmish
open Elmish.React
open Model
open System
open FSharp.Core
open Feliz
open Fetch

type ScenarioIndex = int

type Page =
    | Intro
    | Tutorial
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
    | UserChanged of string * string
    | ShowTutorial
    | Start
    | ToggleRecipe of MethodIndex * RecipeId
    | ViewRecipe of MethodIndex * RecipeId
    | RateMethod of MethodIndex * MethodOpinion
    | FinishScenario of int
    | ErrorOccured of exn
    | Noop

let loadData () =
    fetch "data.json" [] |> Promise.bind (fun r -> r.json<RecommendationScenario array>())

let init =
    {
        User = {
            SessionId = Guid.NewGuid()
            Name = "anonymous"
            Email = ""
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

let sendRecipeToggledInteraction interaction methodIndexFromLeft user index recipeId =
    let interaction = {
        RecipeInteraction = interaction
        RecipeId = recipeId
        MethodId = index.MethodId
        Data = {
            ScenarioIndex = index.ScenarioIndex
            MethodIndexFromLeft = methodIndexFromLeft
            User = user
            TimeStamp = DateTime.UtcNow
        }
    }

    Interactions.send (Interaction.Recipe interaction)

let sendMethodInteraction rating methodIndexFromLeft user index =
    let interaction = {
        MethodRating = rating
        MethodId = index.MethodId
        Data = {
            ScenarioIndex = index.ScenarioIndex
            MethodIndexFromLeft = methodIndexFromLeft
            User = user
            TimeStamp = DateTime.UtcNow
        }
    }

    Interactions.send (Interaction.Method interaction)

let getMethodPosition methodIndex (scenarios: RecommendationScenario[]) =
    let scenario = scenarios.[methodIndex.ScenarioIndex]
    scenario.Recommendations |> Array.findIndex (fun r -> r.Id = methodIndex.MethodId)

let allowedMethodIds = [
    "f2v-256-10-tf-idf-cal";
    "f2v-256-10-tf-idf-mmr";
    "tf-idf-cal";
    "tf-idf-mmr";
    "tf-idf";
    "jaccard";
]

let useMethod (method: RecommendationMethod) =
    List.contains method.Id allowedMethodIds

let update (msg: Msg) (state: State) =
    match msg with
    | LoadData ->
        (state, Cmd.OfPromise.either loadData () DataLoaded ErrorOccured)
    | DataLoaded scenarios ->
        let randomizedScenarios = scenarios |> Array.map (fun s -> { s with Recommendations = s.Recommendations |> Seq.filter useMethod |> randomize |> Seq.toArray })
        let emptySelectedRecipes = scenarios |> Array.mapi (fun index _ -> (index, Set.empty)) |> Map.ofArray
        ({ state with Scenarios = randomizedScenarios; SelectedRecipeIds = emptySelectedRecipes }, Cmd.none)
    | UserChanged (name, email) ->
        ({ state with User = { state.User with Name = name; Email = email } }, Cmd.none)
    | ShowTutorial ->
       ({ state with CurrentPage = Tutorial }, Cmd.none)
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
        let selectedRecipeIds = Map.find index.ScenarioIndex state.SelectedRecipeIds
        let isAlreadySelected = Set.contains recipeId selectedRecipeIds
        let newSelectedRecipeIds =
            if isAlreadySelected
                then Set.remove recipeId selectedRecipeIds
                else Set.add recipeId selectedRecipeIds

        let recipeInteraction = if isAlreadySelected then Disliked else Liked
        let methodPosition = getMethodPosition index state.Scenarios
        let interaction = sendRecipeToggledInteraction recipeInteraction methodPosition state.User index
        let interactionCommand = Cmd.OfPromise.either interaction recipeId (fun _ -> Noop) (fun error -> ErrorOccured error)

        ({ state with SelectedRecipeIds = Map.add index.ScenarioIndex newSelectedRecipeIds state.SelectedRecipeIds }, interactionCommand)
    | ViewRecipe (index, recipeId) ->
        let methodPosition = getMethodPosition index state.Scenarios
        let interaction = sendRecipeToggledInteraction Viewed methodPosition state.User index
        let interactionCommand = Cmd.OfPromise.either interaction recipeId (fun _ -> Noop) (fun error -> ErrorOccured error)

        (state, interactionCommand)
    | RateMethod (index, opinion) ->
        let methodPosition = getMethodPosition index state.Scenarios
        let interaction = sendMethodInteraction opinion methodPosition state.User
        let interactionCommand = Cmd.OfPromise.either interaction index (fun _ -> Noop) (fun error -> ErrorOccured error)

        ({ state with MethodRatings = Map.add index opinion state.MethodRatings }, interactionCommand)
    | ErrorOccured s ->
        window.alert (sprintf "Error occurred, please contact josef.starychfojtu@gmail.com with this and screen of console: %s %s" s.Message s.StackTrace)
        (state, Cmd.none)
    | Noop ->
        (state, Cmd.none)

module Html =
    let styledDiv (className: string) (children: ReactElement seq) =
        Html.div [
            prop.className className
            prop.children children
        ]

let initPage dispatch user =
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
            ]
            Html.styledDiv "row" [
                Html.input [
                    prop.type'.text
                    prop.style [
                        style.marginBottom 10
                    ]
                    prop.onTextChange (fun s -> dispatch <| UserChanged (s, user.Email))
                ]
            ]
            Html.styledDiv "row" [
                Html.label [
                    prop.text "Your email (optional, we will send results of the whole survey to those who will fill in):"
                    prop.style [
                        style.marginRight 15
                    ]
                ]
            ]
            Html.styledDiv "row" [
                Html.input [
                    prop.type'.text
                    prop.style [
                        style.marginBottom 10
                        style.width 350
                    ]
                    prop.onTextChange (fun s -> dispatch <| UserChanged (user.Name, s))
                ]
            ]
            Html.styledDiv "row" [
                Html.button [
                    prop.onClick (fun _ -> dispatch ShowTutorial)
                    prop.className "btn btn-primary"
                    prop.text "Start"
                ]
            ]
        ]
    ]

let tutorialPage dispatch =
    Html.div [
        prop.className "container"
        prop.style [
            style.paddingTop (length.perc 10)
        ]
        prop.children [
            Html.styledDiv "row" [
                Html.h1 "How this works ?"
                Html.p
                    @"The concept of SmartRecipes is that you go shopping and have some ingredients in your basket.
                    SmartRecipes will recommend you recipes based on that, so you can finish your shopping according to them.
                    This survey helps to pick the right method to do that."
                Html.p
                    @"There will be 6 scenarios. In each scenario, your shopping basket is given alongside with some situation about your intentions.
                    Then there are 6 methods (named Alpha, Beta.. and randomly ordered) recommending you recipes. The final application will consist only of a single list, not 6.
                    Thus evaluate them separately in the following way. For each method, go thru its recipes. If you like the recipe based on the basket and scenario, click on it.
                    If you change your mind, just click on it again. At the and of the list, give the method your overall impression (Good/Ok/Bad) at the bottom and move to the next one.
                    There is amount of ingredients actually used in liked recipes displayed above the method rating and number of liked recipes to help you decide."
            ]
            Html.styledDiv "row" [
                Html.button [
                    prop.onClick (fun _ -> dispatch Start)
                    prop.className "btn btn-primary"
                    prop.text "Go to first scenario"
                ]
            ]
        ]
    ]

let renderIngredient ingredient =
    Html.div [
        Html.text ingredient.DisplayLine
    ]

let renderRecipe dispatch index (recipe: Recipe) isSelected =
    let (cardClass, headerClass, bodyClass, anchorClass) =
        if isSelected
            then "card border-success", "card-header text-white bg-success", "card-body text-white bg-success", "text-white"
            else "card", "card-header", "card-body", ""

    let ingredientsInInput = recipe.Ingredients |> Array.filter (fun i -> i.IsInputMatch)
    let otherIngredients = recipe.Ingredients |> Array.filter (fun i -> not i.IsInputMatch)

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
                    for i in ingredientsInInput  do
                        renderIngredient i

                    Html.div[
                        Html.text (sprintf "+ %i other ingredients" otherIngredients.Length)
                    ]

                    Html.div [
                        prop.style [
                            style.marginTop 25
                        ]
                        prop.children [
                            Html.a [
                                prop.href recipe.Uri
                                prop.target "blank"
                                prop.className anchorClass
                                prop.onClick (fun e ->
                                    e.stopPropagation ()
                                    dispatch <|  ViewRecipe (index, recipe.Id))
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
    ("jaccard", "Zeta")
]

let recipeViewCount = 6

let renderMethod dispatch index (method: RecommendationMethod) selectedRecipeIds =
    Html.styledDiv "col-2" [
        Html.h3 (Map.find method.Id methodAliases)
        for recipe in method.Recommendations |> Seq.take recipeViewCount do
            renderRecipe dispatch index recipe (Set.contains recipe.Id selectedRecipeIds)
    ]

let renderMethodRating dispatch methodIndex (method: RecommendationMethod) (currentOpinion: MethodOpinion option) likedRecipeIds inputCount =
    let nameClassName =
        match currentOpinion with
        | Some Great -> "text-success"
        | Some Average -> "text-info"
        | Some Bad -> "text-danger"
        | _ -> ""

    let likedRecipes = method.Recommendations |> Array.where (fun r -> Set.contains r.Id likedRecipeIds)
    let distinctIngredientCount =
        likedRecipes
        |> Seq.collect (fun r -> r.Ingredients)
        |> Seq.filter (fun i -> i.IsInputMatch)
        |> Seq.distinctBy (fun r -> r.FoodstuffId)
        |> Seq.length

    Html.styledDiv "col-2" [
        Html.h3 [
            prop.className nameClassName
            prop.text (Map.find method.Id methodAliases)
        ]
        Html.p [
            prop.style [
                style.marginBottom 0
            ]
            prop.text (sprintf "%i Recipes liked" likedRecipes.Length)
        ]
        Html.p [
            prop.style [
                style.marginBottom 0
            ]
            prop.text (sprintf "%i/%i input ingredient covered" distinctIngredientCount inputCount)
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

let scenarioPage dispatch index totalScenarios scenario selectedRecipeIds methodRatings =
    Html.div [
        Html.styledDiv "container-fluid" [
            Html.h3 (sprintf "Scenario (%i/%i):" (index + 1) totalScenarios)
            Html.paragraph scenario.Description
            Html.h3 "Your basket contains:"
            Html.unorderedList (scenario.Input |> Array.map Html.listItem)

            Html.div [
                prop.className "row"
                prop.style [
                    style.paddingBottom 200
                ]
                prop.children [
                    for method in scenario.Recommendations do
                        let methodIndex = {
                            ScenarioIndex = index
                            MethodId = method.Id
                        }
                        renderMethod dispatch methodIndex method selectedRecipeIds
                ]
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
                    for method in scenario.Recommendations do
                        let methodIndex = {
                            ScenarioIndex = index
                            MethodId = method.Id
                        }
                        let methodRecipes = method.Recommendations |> Array.map (fun r -> r.Id) |> Set.ofArray
                        let likedRecipeIds = Set.intersect selectedRecipeIds methodRecipes |> Set.ofSeq
                        renderMethodRating dispatch methodIndex method (Map.tryFind methodIndex methodRatings) likedRecipeIds scenario.Input.Length
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
    | Intro -> initPage dispatch state.User
    | Tutorial -> tutorialPage dispatch
    | Scenario index ->
        let scenario = state.Scenarios.[index]
        scenarioPage dispatch index (Array.length state.Scenarios) scenario (Map.find index state.SelectedRecipeIds) state.MethodRatings
    | End -> lastPage

Program.mkProgram (fun _ -> (init, Cmd.ofMsg LoadData)) update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run