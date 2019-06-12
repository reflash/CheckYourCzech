module App

open Elmish
open Elmish.React
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation
open Fable.Helpers.React
open Fulma
open Pages

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let urlParser location = parseHash pageParser location

type Subview =
    | Home
    | NounPlurals of NounPlurals.Model
    | NounAccusatives of NounAccusatives.Model
    | AdjectivePlurals of AdjectivePlurals.Model
    | AdjectiveComparatives of AdjectiveComparatives.Model
    | VerbImperatives of VerbImperatives.Model
    | VerbParticiples of VerbParticiples.Model


type Model = {
    Subview: Subview
    ActivePage: Page
}

type Msg = 
    | NounPluralsMsg of NounPlurals.Msg
    | NounAccusativesMsg of NounAccusatives.Msg
    | AdjectivePluralsMsg of AdjectivePlurals.Msg
    | AdjectiveComparativesMsg of AdjectiveComparatives.Msg
    | VerbImperativesMsg of VerbImperatives.Msg
    | VerbParticiplesMsg of VerbParticiples.Msg

let viewPage model dispatch =
    match model with
    | Home ->
        Home.view ()
    | NounPlurals m ->
        NounPlurals.view m (NounPluralsMsg >> dispatch)
    | NounAccusatives m ->
        NounAccusatives.view m (NounAccusativesMsg >> dispatch)
    | AdjectivePlurals m ->
        AdjectivePlurals.view m (AdjectivePluralsMsg >> dispatch)
    | AdjectiveComparatives m ->
        AdjectiveComparatives.view m (AdjectiveComparativesMsg >> dispatch)
    | VerbImperatives m ->
        VerbImperatives.view m (VerbImperativesMsg >> dispatch)
    | VerbParticiples m ->
        VerbParticiples.view m (VerbParticiplesMsg >> dispatch)

let urlUpdate (result:Page option) model =
    let newModel =
        {ActivePage=result |> Option.defaultValue Page.Home; 
         Subview=model.Subview}
    
    match result with
    | None ->
        newModel, Navigation.modifyUrl (Pages.toHash Page.Home)
    | Some Page.Home ->
        newModel, Cmd.none
    | Some Page.NounPlurals ->
        let m, cmd = NounPlurals.init()
        {newModel with Subview = NounPlurals m}, Cmd.map NounPluralsMsg cmd
    | Some Page.NounAccusatives ->
        let m, cmd = NounAccusatives.init()
        {newModel with Subview = NounAccusatives m}, Cmd.map NounAccusativesMsg cmd
    | Some Page.AdjectivePlurals ->
        let m, cmd = AdjectivePlurals.init()
        {newModel with Subview = AdjectivePlurals m}, Cmd.map AdjectivePluralsMsg cmd
    | Some Page.AdjectiveComparatives ->
        let m, cmd = AdjectiveComparatives.init()
        {newModel with Subview = AdjectiveComparatives m}, Cmd.map AdjectiveComparativesMsg cmd
    | Some Page.VerbImperatives ->
        let m, cmd = VerbImperatives.init()
        {newModel with Subview = VerbImperatives m}, Cmd.map VerbImperativesMsg cmd
    | Some Page.VerbParticiples ->
        let m, cmd = VerbParticiples.init()
        {newModel with Subview = VerbParticiples m}, Cmd.map VerbParticiplesMsg cmd

let init result = 
    Logger.setup()
    urlUpdate result {ActivePage = Page.Home; Subview = Subview.Home}

let update msg model =
    match msg, model.Subview with
    | NounPluralsMsg msg, NounPlurals m ->
        let m, cmd = NounPlurals.update msg m
        NounPlurals m, Cmd.map NounPluralsMsg cmd
    | NounAccusativesMsg msg, NounAccusatives m ->
        let m, cmd = NounAccusatives.update msg m
        NounAccusatives m, Cmd.map NounAccusativesMsg cmd
    | AdjectivePluralsMsg msg, AdjectivePlurals m ->
        let m, cmd = AdjectivePlurals.update msg m
        AdjectivePlurals m, Cmd.map AdjectivePluralsMsg cmd
    | AdjectiveComparativesMsg msg, AdjectiveComparatives m ->
        let m, cmd = AdjectiveComparatives.update msg m
        AdjectiveComparatives m, Cmd.map AdjectiveComparativesMsg cmd
    | VerbImperativesMsg msg, VerbImperatives m ->
        let m, cmd = VerbImperatives.update msg m
        VerbImperatives m, Cmd.map VerbImperativesMsg cmd
    | VerbParticiplesMsg msg, VerbParticiples m ->
        let m, cmd = VerbParticiples.update msg m
        VerbParticiples m, Cmd.map VerbParticiplesMsg cmd
    | _, _ ->
        model, Cmd.none

let view model dispatch =
    div [] [ 
        // Menu.view()
        Hero.hero [ Hero.Color IsLight
                    Hero.IsMedium ]
            [ Hero.head [ ]
                [ Menu.view model ]
              Hero.body [ ]
                [ div [ Styles.center "column" ] (viewPage model dispatch) ] ]
        
    ]

Program.mkProgram init update view
|> Program.toNavigable urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
