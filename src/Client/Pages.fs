module Client.Pages

open Elmish.Browser.UrlParser

/// The different pages of the application. If you add a new page, then add an entry here.
[<RequireQualifiedAccess>]
type Page = 
    | Home
    | Multiples

let toHash =
    function
    | Page.Home -> "#home"
    | Page.Multiples -> "#multiples"

/// The URL is turned into a Result.
let pageParser : Parser<Page -> Page,_> =
    oneOf
        [ map Page.Home (s "home")
          map Page.Multiples (s "multiples")]

let urlParser location = parseHash pageParser location