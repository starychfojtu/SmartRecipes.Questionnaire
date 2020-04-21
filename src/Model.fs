module Model

open System

type Ingredient = {
    FoodstuffId: string
    DisplayLine: string
    IsInputMatch: bool
}

type RecipeId = string

type Recipe = {
    Id: RecipeId
    Uri: string // TODO: Should be System.Uri.
    ImageUri: string // TODO: should be System.Uri.
    Name: string
    Ingredients: Ingredient array
}

type RecommendationMethodId = string

type RecommendationMethod = {
    Id: RecommendationMethodId
    Name: string
    Recommendations: Recipe array
}

type Input = string array

type RecommendationScenario = {
    Description: string
    Input: Input
    Recommendations: RecommendationMethod array
}

type User = {
    SessionId: Guid
    Name: string
    Email: string
}

type MethodOpinion =
    | Bad
    | Average
    | Great

type RecipeInteraction = Liked | Disliked | Viewed

type InteractionData = {
    ScenarioIndex: int
    MethodIndexFromLeft: int
    User: User
    TimeStamp: DateTime
}

type RecipeInteractionEvent = {
    RecipeInteraction: RecipeInteraction
    RecipeId: RecipeId
    MethodId: RecommendationMethodId
    Data: InteractionData
}

type MethodInteractionEvent = {
    MethodRating: MethodOpinion
    MethodId: RecommendationMethodId
    Data: InteractionData
}

type Interaction = Recipe of RecipeInteractionEvent | Method of MethodInteractionEvent
