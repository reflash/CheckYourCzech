﻿module Adjective

open FSharp.Data
open Article
open WikiString

type WikiAdjective = HtmlProvider<"https://cs.wiktionary.org/wiki/nový">

let getAdjectiveProvider =
    (+) wikiUrl
    >> WikiAdjective.Load

let hasComparison = 
    ArticleParser.tryGetAdjective
    >> Option.bind (tryGetPart "stupňování")
    >> Option.isSome

let isSyntacticComparison (comparison: string) = comparison.StartsWith "více "

let getPositive =
    getAdjectiveProvider
    >> fun data -> data.Tables.``Stupňování[editovat]``.Rows.[0].tvar

let getComparatives =
    getAdjectiveProvider
    >> fun data -> data.Tables.``Stupňování[editovat]``.Rows.[1].tvar
    >> getForms

let isPositive adjective = (adjective = getPositive adjective)

let hasMorphologicalComparatives = 
    let isMorphological = not << isSyntacticComparison
    
    getComparatives 
    >> Array.filter isMorphological
    >> Seq.isEmpty 
    >> not
