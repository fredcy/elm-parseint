module ParseInt (parseInt, Error) where

import Char
import String


type Error
  = InvalidChar Char
  | OutOfRange Char


{-| Convert string to int assuming given radix. -}
parseInt : Int -> String -> Result Error Int
parseInt radix string =
  parseIntR radix (String.reverse string)


parseIntR : Int -> String -> Result Error Int
parseIntR radix rstring =
  case String.uncons rstring of
    Nothing ->
      Ok 0

    Just ( c, rest ) ->
      Result.map2
        (\ci ri -> ci + ri * radix)
        (intFromChar radix c)
        (parseIntR radix rest)


charOffset : Char -> Char -> Int
charOffset basis c =
  Char.toCode c - Char.toCode basis


isBetween : Char -> Char -> Char -> Bool
isBetween a b c =
  let
    ci = Char.toCode c
  in
    Char.toCode a <= ci && ci <= Char.toCode b


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
