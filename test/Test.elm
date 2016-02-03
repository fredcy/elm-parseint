module Main (..) where

import Array
import Check.Investigator as CI
import Check.Test
import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import Lazy.List exposing (empty, (:::))
import ParseInt exposing (..)
import Random exposing (initialSeed)
import Random.Char
import Random.String
import Shrink exposing (Shrinker)
import String


isErr : Result e a -> Bool
isErr r =
  case r of
    Err _ ->
      True

    Ok _ ->
      False


assertErr : Result a b -> Assertion
assertErr =
  assert << isErr


tests : Test
tests =
  suite
    "basic"
    [ test "decimal" <| assertEqual (Ok 314159) (parseInt "314159")
    , test "simple oct" (assertEqual (Ok 15) (parseIntOct "17"))
    , test "hex" (assertEqual (Ok 2748) (parseIntHex "abc"))
    , test "hex 2" <| assertEqual (Ok 291) (parseIntHex "123")
    , test "hex 3" <| assertEqual (Ok 3735928559) (parseIntHex "DEADBEEF")
    , test "base 32" <| assertEqual (Ok 32767) (parseIntRadix 32 "VVV")
    , test "base 36" <| assertEqual (Ok 1295) (parseIntRadix 36 "ZZ")
    , test "empty string" <| assertEqual (Ok 0) (parseInt "")
    , test "ignore leading zeroes" <| assertEqual (Ok 549) (parseInt "00549")
    , test "oct out of range" <| assert <| isErr (parseIntRadix 8 "8")
    , test "nonnumeric string, base 10" <| assert <| isErr (parseInt "foobar")
    , test "0x prefix is invalid" <| assert <| isErr (parseIntRadix 16 "0xdeadbeef")
    , test "invalid character" <| assertErr <| parseInt "*&^*&^*y"
    , test "invalid radix" <| assertErr <| parseIntRadix 37 "90210"
    ]


checkSuite : Test
checkSuite =
  suite "checks" [ claimMatchesToInt, hexClaim, hexClaim2 ]


genSuite : Test
genSuite =
  suite
    "generator"
    [ test "gen hex" <| assertEqual (Ok "BEEF") (toRadix 16 48879)
    , test "gen dec" <| assertEqual (Ok "314159") (toRadix 10 314159)
    , test "gen binary" <| assertEqual (Ok "100000") (toRadix 2 (2 ^ 5))
    , test "gen oct" <| assertEqual (Ok "30071") (toRadix 8 12345)
    , test "test zero" <| assertEqual (Ok "0") (toRadix 10 0)
    , test "test negative" <| assertEqual (Ok "-123") (toRadix 10 -123)
    , test "gen bad radix" <| assertErr <| toRadix 1 12345
    , test "gen bad radix" <| assertErr <| toRadix 37 12345
      --    , test "bad radix unsafe" <| assertEqual "asplode" <| toRadix' 37 36
    ]


canonResult : Result ParseInt.Error a -> Result String a
canonResult r =
  case r of
    Ok i ->
      Ok i

    Err m ->
      Err (toString m)


claimMatchesToInt : Test
claimMatchesToInt =
  Check.Test.test
    "Matches results of String.toInt"
    (parseInt >> canonResult)
    String.toInt
    stringInvestigator
    100
    (initialSeed 99)


claimMatchesToString : Test
claimMatchesToString =
  Check.Test.test
    "toRadix 10 matches results to toString"
    (toRadix 10 >> canonResult)
    (Ok << toString)
    (CI.rangeInt Random.minInt Random.maxInt)
    100
    (initialSeed 134)


{-| Convert i to string with given radix, then back again to int.
-}
roundTrip : ( Int, Int ) -> Result Error Int
roundTrip ( radix, i ) =
  toRadix radix i |> Result.withDefault "" >> parseIntRadix radix


claimCrossCheck : Test
claimCrossCheck =
  Check.Test.test
    "parseIntRadix inverts toRadix for non-negative Ints"
    roundTrip
    (Ok << snd)
    (CI.tuple
      ( CI.rangeInt 2 36
      , CI.rangeInt 0 Random.maxInt
      )
    )
    100
    (initialSeed 99)


{-| Integer division that handles large numerators (mostly) correctly.  See
<https://github.com/elm-lang/core/issues/92>. The `toFloat` conversion can lose
precision and cause the result to be off as well.
-}
(//) : Int -> Int -> Int
(//) x y =
  floor (Basics.toFloat x / Basics.toFloat y)


hexClaim : Test
hexClaim =
  Check.Test.test
    "Hex conversion, dropping rightmost char results in dividing by 16"
    -- got
    (String.dropRight 1 >> parseIntRadix 16)
    -- expected
    (parseIntRadix 16 >> Result.map (\i -> i // 16))
    hexStringInvestigator
    100
    (initialSeed 88)


hexClaim2 : Test
hexClaim2 =
  Check.Test.test
    "Hex conversion, adding '0' to right results in multiplying by 16"
    (parseIntRadix 16 >> Result.map (\i -> i * 16))
    ((\s -> s ++ "0") >> parseIntRadix 16)
    hexStringInvestigator
    100
    (initialSeed 88)


randomDigitChar : Random.Generator Char
randomDigitChar =
  Random.Char.char 48 57


randomHexChar : Random.Generator Char
randomHexChar =
  let
    hexChars =
      Array.fromList [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' ]
  in
    Random.map (\i -> Array.get i hexChars |> Maybe.withDefault 'X') (Random.int 0 15)


{-| Shrink by successively removing the head of the string. Do not include empty string.
-}
shrinker : Shrink.Shrinker String
shrinker s =
  case String.uncons s |> Debug.log "shrinker" of
    Nothing ->
      empty

    Just ( _, rest ) ->
      if rest == "" then
        empty
      else
        rest ::: shrinker rest


{-| Generate random digit strings. Limit to 16 chars to keep full precision in javascript.
-}
stringInvestigator : CI.Investigator String
stringInvestigator =
  CI.investigator
    (Random.String.rangeLengthString 1 16 randomDigitChar)
    shrinker


{-| Generate strings containing random hexidecimal characters.  Since javascript
 loses precision for numbers over 2 ^ 54 (40000000000000 in hex) we limit the
 strings to 13 chars long to stay just under that.
-}
hexStringInvestigator : CI.Investigator String
hexStringInvestigator =
  CI.investigator
    (Random.String.rangeLengthString 1 13 randomHexChar)
    shrinker


main : Element
main =
  elementRunner
    <| suite
        "all"
        [ tests
        , checkSuite
        , genSuite
        , claimMatchesToString
        , claimCrossCheck
        ]
