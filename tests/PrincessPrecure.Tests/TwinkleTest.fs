namespace PrincessPrecure.Tests

open System
open Persimmon
open UseTestNameByReflection
open PrincessPrecure.Twinkle

module TwinkleTest =

  let ``unit type should hamminged "()"`` = test {
    do! assertEquals "()" (hamming ())
  }

  let ``bool should be hamminged lower case`` = test {
    do! assertEquals "true" (hamming true)
    do! assertEquals "false" (hamming false)
  }

  let ``hamminger should hamming string`` = test {
    do! assertEquals "\"\"" (hamming "")
    do! assertEquals "\"a\"" (hamming "a")
    do! assertEquals "null" (hamming (null: string))
  }

  let ``hamminger should hamming array in a row`` = test {
    do! assertEquals "[|1; 2; 3|]" (hamming [| 1; 2; 3 |])
  }

  let ``hamminger should hamming list in a row`` = test {
    do! assertEquals "[1; 2; 3]" (hamming [ 1; 2; 3 ])
  }

  let ``hamminger should hamming seq in a row`` = test {
    do! assertEquals "seq [1; 2; 3]" (hamming (seq { 1 .. 3 }))
    do! assertEquals "seq [1; 2; 3]" (hamming (seq { 1 .. 3 }))
  }

  type TestRecord = {
    Field1: int
    Field2: string
  }

  let ``hamminger should hamming record in a row`` = test {
    do! assertEquals """{ Field1 = 1; Field2 = "test" }""" (hamming { Field1 = 1; Field2 = "test" })
  }

  let ``hamminger should hamming tuple`` = test {
    do! assertEquals "(1, 2)" (hamming (1, 2))
    do! assertEquals "(1, (2, 3))" (hamming (1, (2, 3)))
    do! assertEquals "(1, seq [1; 2])" (hamming (1, seq { 1 .. 2 }))
  }

  type TestUnion =
    | Case1
    | Case2 of bool
    | Case3 of int * int

  let ``hamminger should hamming DU in a row`` = test {
    do! assertEquals "Case1" (hamming Case1)
    do! assertEquals "Case1" (hamming  (box Case1))
    do! assertEquals "Case2(true)" (hamming (Case2 true))
    do! assertEquals "Case3(0, 1)" (hamming (Case3(0, 1)))
  }

  let ``hamminger should hamming option`` = test {
    do! assertEquals "None" (hamming None)
    do! assertEquals "Some(true)" (hamming (Some true))
  }

  let ``hamminger should hamming Nullable`` = test {
    do! assertEquals "null" (hamming (Nullable()))
    do! assertEquals "1" (hamming (Nullable<int>(1)))
  }
