﻿module Pattern

open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React

[<Literal>]
let PatternUnset = ""

type Model = { 
    Patterns : string list option
    SelectedPattern : string option
}

type Msg =
    | SetPatterns of string list option
    | SelectPattern of string option

let init patterns =
    { Patterns = patterns
      SelectedPattern = None }

let update msg model =
    match msg with
    | SetPatterns patterns ->
        { model with Patterns = patterns; SelectedPattern = None }
    | SelectPattern pattern ->
        { model with SelectedPattern = pattern }

let view model dispatch =
    let handleChangePattern (event: FormEvent) =
        let translate = function | PatternUnset -> None | x -> Some x
        dispatch (SelectPattern (translate !!event.target?value))
        
    let options = 
        model.Patterns
        |> Option.defaultValue []
        |> Seq.map Markup.simpleOption
        |> Seq.append [ Markup.option PatternUnset "Any" ]

    let selectedValue = model.SelectedPattern |> Option.defaultValue "Any"

    div [ ClassName "pattern-filter" ] 
        [
            div [ ClassName "pattern-filter-label" ] 
                [
                    Markup.label Styles.whiteLabel (str "Pattern") 
                ]

            div [ Styles.select ] 
                [
                    Markup.select selectedValue handleChangePattern options
                ]
        ]
