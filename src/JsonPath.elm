module JsonPath exposing (..)

import Json.Decode
import Parser


type alias Path =
    List Selector


type Selector
    = Wildcard
    | Slice { start : Int, maybeEnd : Maybe Int, step : Int }
    | Indices Int (List Int)
    | Keys String (List String)


type Error
    = PathParsingError (List Parser.DeadEnd)
    | JsonDecodingError Json.Decode.Error
    | IndexNotFound Int
