module Main (..) where

import Check.Investigator as CI
import Check.Test
import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import ParseInt exposing (parseInt)
import Random exposing (initialSeed)
import Random.Char
import Random.String
import Shrink
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
    [ test "decimal" <| assertEqual (Ok 314159) (parseInt 10 "314159")
    , test "simple oct" (assertEqual (Ok 15) (parseInt 8 "17"))
    , test "hex" (assertEqual (Ok 2748) (parseInt 16 "abc"))
    , test "hex 2" <| assertEqual (Ok 291) (parseInt 16 "123")
    , test "ignore leading zeroes" <| assertEqual (Ok 549) (parseInt 10 "00549")
    , test "oct out of range" <| assert <| isErr (parseInt 8 "8")
    , test "nonnumeric string, base 10" <| assert <| isErr (parseInt 10 "foobar")
    , test "0x prefix is invalid" <| assert <| isErr (parseInt 16 "0xdeadbeef")
    , test "invalid character" <| assertErr <| parseInt 10 "*&^*&^*y"
    ]


checkSuite : Test
checkSuite =
  suite "checks" [ claimMatchesToInt ]


canonResult : Result ParseInt.Error Int -> Result String Int
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
    (parseInt 10 >> canonResult)
    String.toInt
    stringInvestigator
    10
    (initialSeed 99)


randomDigitChar : Random.Generator Char
randomDigitChar =
  Random.Char.char 48 57


stringInvestigator : CI.Investigator String
stringInvestigator =
  CI.investigator
    (Random.String.rangeLengthString 1 10 randomDigitChar)
    (Shrink.string)


main : Element
main =
  elementRunner <| suite "all" [ tests, checkSuite ]
