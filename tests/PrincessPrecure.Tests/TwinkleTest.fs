namespace PrincessPrecure.Tests

open System
open Persimmon
open UseTestNameByReflection
open PrincessPrecure.Twinkle

module TwinkleTest =

  let ``print unit`` = test {
    do! assertEquals "()" (hamming ())
  }

  let ``print bool`` = test {
    do! assertEquals "true" (hamming true)
    do! assertEquals "false" (hamming false)
  }

  let ``print string`` = test {
    do! assertEquals "\"\"" (hamming "")
    do! assertEquals "\"a\"" (hamming "a")
    do! assertEquals "null" (hamming (null: string))
  }

  let ``print array`` = test {
    do! assertEquals "[|1; 2; 3|]" (hamming [| 1; 2; 3 |])
  }

  let ``print list`` = test {
    do! assertEquals "[1; 2; 3]" (hamming [ 1; 2; 3 ])
  }

  let ``print seq`` = test {
    do! assertEquals "seq [1; 2; 3]" (hamming (seq { 1 .. 3 }))
    do! assertEquals "seq [1; 2; 3]" (hamming (seq { 1 .. 3 }))
  }

  type TestRecord = {
    Field1: int
    Field2: string
  }

  let ``print record`` = test {
    do! assertEquals """{ Field1 = 1; Field2 = "test" }""" (hamming { Field1 = 1; Field2 = "test" })
  }

  let ``print tuple`` = test {
    do! assertEquals "(1, 2)" (hamming (1, 2))
    do! assertEquals "(1, (2, 3))" (hamming (1, (2, 3)))
    do! assertEquals "(1, seq [1; 2])" (hamming (1, seq { 1 .. 2 }))
  }

  type TestUnion =
    | Case1
    | Case2 of bool
    | Case3 of int * int

  let ``print DU`` = test {
    do! assertEquals "Case1" (hamming Case1)
    do! assertEquals "Case1" (hamming  (box Case1))
    do! assertEquals "Case2(true)" (hamming (Case2 true))
    do! assertEquals "Case3(0, 1)" (hamming (Case3(0, 1)))
  }

  let ``print option value`` = test {
    do! assertEquals "None" (hamming None)
    do! assertEquals "Some(true)" (hamming (Some true))
  }

  let ``print Nullable`` = test {
    do! assertEquals "null" (hamming (Nullable()))
    do! assertEquals "1" (hamming (Nullable<int>(1)))
  }
