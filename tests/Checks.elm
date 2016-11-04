module Checks exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import ParseInt exposing (..)
import Random
import String


all =
    Test.concat
        [ testMatchesToString
        , testMatchesToInt
        , testToRadixVsToString
        , testRoundTrip
        ]


testMatchesToString : Test
testMatchesToString =
    Test.fuzz (Fuzz.intRange 0 Random.maxInt) "basic parseInt" <|
        \i -> Expect.equal (Ok i) (toString i |> parseInt)


testMatchesToInt : Test
testMatchesToInt =
    Test.fuzz (Fuzz.intRange 0 Random.maxInt) "parseInt vs toInt" <|
        \i ->
            let
                intString =
                    toString i
            in
                Expect.equal
                    (String.toInt intString)
                    (parseInt intString |> Result.mapError toString)


testToRadixVsToString : Test
testToRadixVsToString =
    Test.fuzz Fuzz.int "toRadix vs toString" <|
        \i -> Expect.equal (toRadix 10 i |> Result.withDefault "") (toString i)


fuzzerRadixInt : Fuzz.Fuzzer ( Int, Int )
fuzzerRadixInt =
    Fuzz.map2 (,)
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
