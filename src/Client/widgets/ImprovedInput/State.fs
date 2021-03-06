module Client.Widgets.ImprovedInput.State

open Elmish
open Fable.Core.JsInterop
open Fable.Import.Browser

open Types

let init inputId = { 
    Value = "" 
    CursorStart = 1
    CursorEnd = 1
    InputId = inputId
}

let update msg model =
    match msg with
    | ChangeInput e -> 
        { model with Value = e.target?value; CursorStart = e.target?selectionStart; CursorEnd = e.target?selectionEnd}, Cmd.none
    | SetInput s ->
        { model with Value = s}, Cmd.none
    | AddSymbol c ->
        let input : HTMLInputElement = !!document.getElementById(model.InputId)
        let cursorStart = int input.selectionStart
        let cursorEnd = int input.selectionEnd
        let startSection = model.Value.[0..cursorStart-1]
        let endSection = model.Value.[cursorEnd..]
        { model with Value = startSection + string c + endSection; CursorStart = cursorStart; CursorEnd = cursorEnd }, Cmd.none
    | FocusInput ->
        document.getElementById(model.InputId).focus()
        model, Cmd.none
    | Reset ->
        init model.InputId, Cmd.none
