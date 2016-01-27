import Graphics.Element exposing (Element)
import ElmTest exposing (..)

import ParseInt exposing (parseInt)

isErr : Result e a -> Bool
isErr r =
  case r of
    Err _ -> True
    Ok _ -> False

tests : Test
tests =
  suite "Octal"
          [ test "simple" (assertEqual (Ok 15) (parseInt 8 "17"))
          , test "hex" (assertEqual (Ok 2748) (parseInt 16 "abc"))
          , test "oct bad" <| assert <| isErr (parseInt 8 "8")
          ]

main : Element
main =
  elementRunner tests
                
