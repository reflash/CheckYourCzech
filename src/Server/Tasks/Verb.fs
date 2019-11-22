module Tasks.Verb

open FSharp.Control.Tasks.V2
open Giraffe
open Storage
open Microsoft.AspNetCore.Http
open Tasks.Utils
open Conjugation
open Common.Utils
open Microsoft.Extensions.Logging
open GrammarCategories

let getVerbImperativesTask next (ctx : HttpContext) =
    task {
        let classFromQuery = ctx.GetQueryStringValue "class"
        let classFilter = getAzureFilter "Class" Int classFromQuery

        let patternFromQuery = ctx.GetQueryStringValue "pattern"
        let patternFilter = getAzureFilter "Pattern" String patternFromQuery

        let filters = 
            [ classFilter; patternFilter ]
            |> Seq.choose id
            
        let verb = tryGetRandom<VerbImperative.VerbImperative> "verbimperatives" filters

        let getTask (verb: VerbImperative.VerbImperative) = 
            let indicative = getAs<string> verb.Indicative
            let imperatives = getAs<string []> verb.Imperatives
            Task(indicative, imperatives)

        let task = verb |> Option.map getTask |> Option.toObj 
        return! Successful.OK task next ctx
    }

let getVerbParticiplesTask next (ctx: HttpContext) =
    task {
        let patternFromQuery = ctx.GetQueryStringValue "pattern"
        let patternFilter = getAzureFilter "Pattern" String patternFromQuery

        let regularityFromQuery = ctx.GetQueryStringValue "isRegular"
        let regularityFilter = getAzureFilter "IsRegular" Bool regularityFromQuery

        let filters =
            [ patternFilter; regularityFilter ] 
            |> Seq.choose id
        
        let verb = tryGetRandom<VerbParticiple.VerbParticiple> "verbparticiples" filters

        let getTask (verb: VerbParticiple.VerbParticiple) = 
            let infinitive = getAs<string> verb.Infinitive
            let participles = getAs<string []> verb.Participles
            Task(infinitive, participles)

        let task = verb |> Option.map getTask |> Option.toObj 
        return! Successful.OK task next ctx
    }

[<AllowNullLiteral>]
type ConjugationTask(word, answers, pronoun) = 
    inherit Task(word, answers)
    member this.Pronoun: string = pronoun

let getVerbConjugationTask next (ctx: HttpContext) =
    task {
        // let logger = ctx.GetLogger()

        let patternFromQuery = ctx.GetQueryStringValue "pattern"
        let patternFilter = getAzureFilter "Pattern" String patternFromQuery

        let filters =
            [ patternFilter ] 
            |> Seq.choose id

        addTypeDescriptor<Number, Person>()
        
        let verb = tryGetRandom<VerbConjugation.VerbConjugation> "verbconjugation" filters
        let getTask (verb: VerbConjugation.VerbConjugation) = 
            let number = getRandomNumber()
            let person = getRandomPerson()
            match (number, person) with 
            | Some n, Some p ->
                let infinitive = getAs<string> verb.Infinitive
                let conjugations = getAs<ConjugationMapping> verb.Conjugations
                let answers = conjugations.Item((n, p))
                let pronoun = getPronounString n p
                Some (ConjugationTask(infinitive, answers, pronoun))
            | _, _ -> 
                None
            

        let task = verb |> Option.bind getTask |> Option.toObj 
        return! Successful.OK task next ctx
    }