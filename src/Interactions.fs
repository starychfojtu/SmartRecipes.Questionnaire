module Interactions

open Fetch
open Model

let writeOnlyStorageKey = "https://smartrecipes.table.core.windows.net/survey?sv=2019-02-02&ss=t&srt=sco&sp=wac&se=2020-12-31T19:59:53Z&st=2020-04-21T10:59:53Z&spr=https,http&sig=o2L16uM%2BsLy3kpQC%2BiK2YWRlcwcD0cad06P4%2F7dEIvE%3D"

let send (interaction: Interaction) =
    fetch "https://google.com" []