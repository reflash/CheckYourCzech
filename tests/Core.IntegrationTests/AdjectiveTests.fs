﻿module AdjectiveTests

open Xunit
open Adjective

let equals (expected: 'T) (actual: 'T) = Assert.Equal<'T>(expected, actual)

[<Fact>]
let ``Gets positive - when positive``() = 
    "dobrý"
    |> getPositive
    |> equals "dobrý"

[<Fact>]
let ``Gets positive - when comparative``() = 
    "lepší"
    |> getPositive
    |> equals "dobrý"

[<Fact>]
let ``Gets positive - when superlative``() = 
    "nejlepší"
    |> getPositive
    |> equals "dobrý"

[<Fact>]
let ``Validates proper adjective``() =
    "dobrý"
    |> isValid 
    |> Assert.True

[<Fact>]
let ``Detects regular adjective - stem ends hard``() = 
    "žlutý"
    |> isRegular
    |> Assert.True
    
[<Fact>]
let ``Detects regular adjective - stem ends soft``() = 
    "milý"
    |> isRegular
    |> Assert.True

[<Fact>]
let ``Detects regular adjective - stem alternates``() = 
    "dogmatický"
    |> isRegular
    |> Assert.True

[<Fact>]
let ``Detects regular adjective - no comparative``() = 
    "vroucí"
    |> isRegular
    |> Assert.True

[<Fact>]
let ``Detects irregular adjective``() = 
    "dobrý"
    |> isRegular
    |> Assert.False

[<Fact>]
let ``Invalidates improper adjective - no Czech``() =
    "good"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper adjective - no adjective``() =
    "nazdar"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper adjective - no comparison``() =
    "občasný"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper adjective - not a positive form - comparative``() =
    "horší"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper adjective - not a positive form - superlative``() =
    "nejširší"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper adjective - no comparatives``() =
    "optimální"
    |> isValid
    |> Assert.False