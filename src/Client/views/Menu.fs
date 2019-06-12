module Menu

open Fable.Helpers.React
open Fulma
open Pages

let view activePage =
    let tab page text = 
        Tabs.tab [ Tabs.Tab.IsActive (activePage == page) ]
            [ Markup.viewLink page text ]
            
    Tabs.tabs [ Tabs.IsBoxed
                Tabs.IsCentered ]
        [ 
            tab Page.Home "Home"
            tab Page.NounPlurals "Noun plurals"
            tab Page.NounAccusatives "Noun accusatives"
            tab Page.AdjectivePlurals "Adjective plurals"
            tab Page.AdjectiveComparatives "Adjective comparatives"
            tab Page.VerbImperatives "Verb imperatives" 
            tab Page.VerbParticiples "Verb participles"
        ]
