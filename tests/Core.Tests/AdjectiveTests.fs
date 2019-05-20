﻿module AdjectiveTests

open Xunit
open Adjective

let equals (expected: 'T) (actual: 'T) = Assert.Equal<'T>(expected, actual)

[<Fact>]
let ``Detects syntactic comparison``() = 
    "více hyzdící"
    |> isSyntacticComparison
    |> Assert.True

[<Fact>]
let ``Detects morphological comparison``() = 
    "novější"
    |> isSyntacticComparison
    |> Assert.False

[<Theory>]
[<InlineData "bratrův">]
[<InlineData "sestřin">]
let ``Detects possessive`` adjective = 
    adjective
    |> isPossessive
    |> Assert.True

[<Fact>]
let ``Detects non possessive``() = 
    "nový"
    |> isPossessive
    |> Assert.False
    
[<Theory>]
[<InlineData "dobrý">]
[<InlineData "dobrá">]
[<InlineData "dobré">]
let ``Detects hard positive`` adjective = 
    adjective
    |> isHardPositive
    |> Assert.True

[<Theory>]
[<InlineData "lepší">]
[<InlineData "moderní">]
let ``Detects not hard positive`` adjective = 
    adjective
    |> isHardPositive
    |> Assert.False

[<Fact>]
let ``Detects soft positive``() = 
    "moderní"
    |> isSoftPositive
    |> Assert.True

[<Theory>]
[<InlineData "černý">]
[<InlineData "modernější">]
let ``Detects not soft positive`` adjective = 
    adjective
    |> isSoftPositive
    |> Assert.False
