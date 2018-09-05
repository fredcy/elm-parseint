module Checks exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import ParseInt exposing (..)
import Random
import String


all =
    describe "fuzz tests"
        [ testMatchesToString
        , testMatchesToInt
        , testToRadixVsToString
        , testRoundTrip
        ]


testMatchesToString : Test
testMatchesToString =
    Test.fuzz (Fuzz.intRange 0 Random.maxInt) "basic parseInt" <|
        \i -> Expect.equal (Ok i) (String.fromInt i |> parseInt)


testMatchesToInt : Test
testMatchesToInt =
    Test.fuzz (Fuzz.intRange 0 Random.maxInt) "parseInt vs toInt" <|
        \i ->
            let
                intString =
                    String.fromInt i
            in
                Expect.equal
                    (String.toInt intString)
                    (parseInt intString |> Result.toMaybe)


testToRadixVsToString : Test
testToRadixVsToString =
    Test.fuzz Fuzz.int "toRadix vs String.fromInt" <|
        \i -> Expect.equal (toRadix 10 i |> Result.withDefault "") (String.fromInt i)


fuzzerRadixInt : Fuzz.Fuzzer ( Int, Int )
fuzzerRadixInt =
    Fuzz.map2 Tuple.pair
        (Fuzz.intRange 2 36)
        (Fuzz.intRange 0 Random.maxInt)


testRoundTrip : Test
testRoundTrip =
    Test.fuzz fuzzerRadixInt "round trip" <|
        \( radix, i ) ->
            Expect.equal (Ok i) <|
                (toRadix radix i
                    |> Result.withDefault ""
                    |> parseIntRadix radix
                )
