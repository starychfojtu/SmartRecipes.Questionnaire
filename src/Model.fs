module Model

open System

type Ingredient = {
    DisplayLine: string
    IsInputMatch: bool
}

type RecipeId = string

type Recipe = {
    Id: RecipeId
    Uri: Uri
    Name: string
    Ingredients: Ingredient list
}

type RecommendationMethodId = string

type RecommendationMethod = {
    Id: RecommendationMethodId
    Name: string
    Recommendations: Recipe list
}

type User = {
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