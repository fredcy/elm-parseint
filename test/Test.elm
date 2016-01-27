import Graphics.Element exposing (Element)
import ElmTest exposing (..)

import ParseInt exposing (parseInt)

isErr : Result e a -> Bool
isErr r =
  case r of
    Err _ -> True
    Ok _ -> False

assertErr : Result a b -> Assertion
assertErr =
  assert << isErr

tests : Test
tests =
  suite "basic"
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

main : Element
main =
  elementRunner tests
                
