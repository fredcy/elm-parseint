module ParseInt (parseInt, parseIntRadix, Error(..)) where

{-| Functions for converting a String value to Int.

    parseInt "314159" == Ok 314159
    parseIntRadix 16 "DEADBEEF" = Ok 3735928559
    parseInt "foo" = Err (OutOfRange 'o')

# Functions
@docs parseInt, parseIntRadix

# Errors
@docs Error
-}

import Char
import Result exposing (andThen)
import String


{-| Possible Result.Err returns from the parseInt functions. -}
type Error
  = InvalidChar Char
  | OutOfRange Char
  | InvalidRadix Int


{-| Convert String to Int assuming base 10. -}
parseInt : String -> Result Error Int
parseInt =
  parseIntRadix 10


{-| Convert String to Int assuming given radix. Radix can be any of [2..36].
-}
parseIntRadix : Int -> String -> Result Error Int
parseIntRadix radix string =
  if 2 <= radix && radix <= 36 then
    parseIntR radix (String.reverse string)
  else
    Err (InvalidRadix radix)


parseIntR : Int -> String -> Result Error Int
parseIntR radix rstring =
  case String.uncons rstring of
    Nothing ->
      Ok 0

    Just ( c, rest ) ->
      intFromChar radix c
        `andThen` (\ci ->
                    parseIntR radix rest
                      `andThen` (\ri -> Ok (ci + ri * radix))
                  )


{-| Offset of character from basis character in the ASCII table.
-}
charOffset : Char -> Char -> Int
charOffset basis c =
  Char.toCode c - Char.toCode basis


{-| Test if character falls in given range (inclusive of the limits) in the ASCII table.
-}
isBetween : Char -> Char -> Char -> Bool
isBetween lower upper c =
  let
    ci = Char.toCode c
  in
    Char.toCode lower <= ci && ci <= Char.toCode upper


{-| Convert alphanumeric character to int value as a "digit", validating against
the given radix. Alphabetic characters past "F" are extended in the natural way:
'G' == 16, 'H' == 17, etc. Upper and lower case are treated the same.
-}
intFromChar : Int -> Char -> Result Error Int
intFromChar radix c =
  let
    toInt =
      if isBetween '0' '9' c then
        Ok (charOffset '0' c)
      else if isBetween 'a' 'z' c then
        Ok (10 + charOffset 'a' c)
      else if isBetween 'A' 'Z' c then
        Ok (10 + charOffset 'A' c)
      else
        Err (InvalidChar c)

    validInt i =
      if i < radix then
        Ok i
      else
        Err (OutOfRange c)
  in
    toInt `Result.andThen` validInt
