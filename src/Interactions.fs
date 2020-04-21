module Interactions

open System
open Model
open Thoth.Fetch
open Thoth.Json

let writeOnlyStorageUrl = "https://smartrecipes.table.core.windows.net/survey?sv=2019-02-02&ss=t&srt=sco&sp=wac&se=2020-12-31T19:59:53Z&st=2020-04-21T10:59:53Z&spr=https,http&sig=o2L16uM%2BsLy3kpQC%2BiK2YWRlcwcD0cad06P4%2F7dEIvE%3D"

let send (interaction: Interaction) = promise {
        let data =
            match interaction with
            | Interaction.Recipe recipeInteraction ->
                let encodedInteraction = match recipeInteraction.RecipeInteraction with | Liked -> "liked" | Disliked -> "disliked" | Viewed -> "viewed"
                Encode.object [
                    "recipeInteraction", Encode.string encodedInteraction
                    "recipeId", Encode.string recipeInteraction.RecipeId
                    "methodId", Encode.string recipeInteraction.MethodId
                    "scenarioIndex", Encode.int recipeInteraction.Data.ScenarioIndex
                    "methodIndexFromLeft", Encode.int recipeInteraction.Data.MethodIndexFromLeft
                    "sessionId", Encode.string (recipeInteraction.Data.User.SessionId.ToString())
                    "userName", Encode.string recipeInteraction.Data.User.Name
                    "userEmail", Encode.string recipeInteraction.Data.User.Email
                    "timeStamp", Encode.string (recipeInteraction.Data.TimeStamp.ToString("o"))
                    "PartitionKey", Encode.string "mainPartition"
                    "RowKey", Encode.string (Guid.NewGuid().ToString())
                ]
            | Interaction.Method methodInteraction ->
                let encodedInteraction = match methodInteraction.MethodRating with | Great -> "great" | Average -> "average" | Bad -> "bad"
                Encode.object [
                    "methodInteraction", Encode.string encodedInteraction
                    "methodId", Encode.string methodInteraction.MethodId
                    "scenarioIndex", Encode.int methodInteraction.Data.ScenarioIndex
                    "methodIndexFromLeft", Encode.int methodInteraction.Data.MethodIndexFromLeft
                    "sessionId", Encode.string (methodInteraction.Data.User.SessionId.ToString())
                    "userName", Encode.string methodInteraction.Data.User.Name
                    "userEmail", Encode.string methodInteraction.Data.User.Email
                    "timeStamp", Encode.string (methodInteraction.Data.TimeStamp.ToString("o"))
                    "PartitionKey", Encode.string "mainPartition"
                    "RowKey", Encode.string (Guid.NewGuid().ToString())
                ]

        do! Fetch.post(writeOnlyStorageUrl, data)
    }