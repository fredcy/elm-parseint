port module HtmlMain exposing (..)

import Checks
import Tests
import Test
import Test.Runner.Html
import Test.Runner.Html.App


main : Test.Runner.Html.TestProgram
main =
    Test.concat [ Tests.all, Checks.all ]
        |> Test.Runner.Html.run
