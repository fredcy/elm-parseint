port module Main exposing (..)

import Checks
import Tests
import Test
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Test.Runner.Node.TestProgram
main =
    run emit <| Test.concat [ Tests.all, Checks.all ]


port emit : ( String, Value ) -> Cmd msg
