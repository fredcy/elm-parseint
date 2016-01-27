module ParseInt (..) where

import Char
import String


parseInt : Int -> String -> Result String Int
parseInt base string =
  parseIntR base (String.reverse string)


parseIntR : Int -> String -> Result String Int
parseIntR base rstring =
  case String.uncons rstring of
    Nothing ->
      Ok 0

    Just ( c, rest ) ->
      let
        ci = intFromChar base c |> Debug.log "ci"
      in
        case ci of
          Err msg ->
            Err msg

          Ok i ->
            let
              r = parseIntR base rest
            in
              case r of
                Err msg ->
                  Err msg

                Ok ri ->
                  Ok (ri * base + i)


charOffset : Char -> Char -> Int
charOffset basis c =
  Char.toCode c - Char.toCode basis


intFromChar : Int -> Char -> Result String Int
intFromChar base c =
  let
    toInt c =
      if Char.isDigit c then
        Ok (charOffset '0' c)
      else if Char.isHexDigit c then
        Ok (charOffset 'a' (Char.toLower c) + 10)
      else
        Err ("invalid char: " ++ toString c)
    validInt i =
      if i < base then Ok i
      else Err "out of range"
  in
    toInt c `Result.andThen` validInt
