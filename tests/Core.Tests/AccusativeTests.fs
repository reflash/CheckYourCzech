﻿module AccusativeTests

open Xunit
open Accusative

let equals (expected: 'T) (actual: 'T) = Assert.Equal<'T>(expected, actual)

[<Fact>]
let ``Validates proper noun``() =
    "panda"
    |> isValid 
    |> Assert.True

[<Fact>]
let ``Invalidates improper noun - no Czech``() =
    "good"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper noun - no noun``() =
    "koukat"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper noun - no declension``() =
    "antilopu"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper noun - no gender``() =
    "trosečník"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper noun - incorrect gender``() =
    "benediktin"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper noun - no nominatives``() =
    "záda"
    |> isValid
    |> Assert.False

[<Fact>]
let ``Invalidates improper noun - multiple nominatives``() =
    "temeno"
    |> isValid
    |> Assert.False
