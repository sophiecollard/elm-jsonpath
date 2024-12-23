module JsonPath exposing (..)

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
    | IndexNotFound Cursor Int
    | KeyNotFound Cursor String
    | NotAJsonArray Cursor
    | NotAJsonArrayNorAnObject Cursor


type alias Cursor =
    List CursorOp


type CursorOp
    = DownIndex Int
    | DownKey String
