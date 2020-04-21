module Model

open System

type Ingredient = {
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

type MousePosition = {
    X: int
    Y: int
}

type RecipePosition = {
    MethodPositionFromLeft: int
    RecipePositionInMethod: int
}

type Metadata = {
    MousePosition: MousePosition
    RecipePosition: RecipePosition
    TimeStamp: DateTime
}

type InteractionData = {
    ScenarioIndex: int
    Metadata: Metadata
    User: User
}

type RecipeInteractionEvent = {
    RecipeInteraction: RecipeInteraction
    RecipeId: RecipeId
    RecipeUri: Uri
    MethodId: RecommendationMethodId
    Data: InteractionData
}

type MethodInteractionEvent = {
    MethodRating: MethodOpinion
    MethodId: RecommendationMethodId
    Data: InteractionData
}

type Interaction = RecipeInteractionEvent | MethodInteractionEvent
