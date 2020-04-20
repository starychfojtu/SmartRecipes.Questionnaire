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
}

type Interaction = Liked | Disliked

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

type InteractionEvent = {
    Interaction: Interaction
    MethodId: RecommendationMethodId
    RecipeId: RecipeId
    RecipeUri: Uri
    UserName: string
    Metadata: Metadata
}
