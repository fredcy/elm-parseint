module ParseInt (..) where

import Char
import String


type Error
  = InvalidChar Char
  | OutOfRange Int


parseInt : Int -> String -> Result Error Int
parseInt base string =
  parseIntR base (String.reverse string)


parseIntR : Int -> String -> Result Error Int
parseIntR base rstring =
  case String.uncons rstring of
    Nothing ->
      Ok 0

    Just ( c, rest ) ->
      Result.map2
        (\ci ri -> ci + ri * base)
        (intFromChar base c)
        (parseIntR base rest)


charOffset : Char -> Char -> Int
charOffset basis c =
  Char.toCode c - Char.toCode basis


intFromChar : Int -> Char -> Result Error Int
intFromChar base c =
  let
    toInt c =
      if Char.isDigit c then
        Ok (charOffset '0' c)
      else if Char.isHexDigit c then
        Ok (charOffset 'a' (Char.toLower c) + 10)
      else
        Err (InvalidChar c)

    validInt i =
      if i < base then
        Ok i
      else
        Err (OutOfRange i)
  in
    toInt c `Result.andThen` validInt
