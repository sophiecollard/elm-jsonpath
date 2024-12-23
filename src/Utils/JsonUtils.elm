module Utils.JsonUtils exposing (..)

import Json.Decode exposing (Value, decodeValue)
import JsonPath exposing (Error(..))
import Utils.ListUtils exposing (traverseMaybe)


asList : Value -> Maybe (List Value)
asList json =
    -- Attempts to decode a JSON value as a List
    json
        |> decodeValue (Json.Decode.list Json.Decode.value)
        |> Result.toMaybe


flattenIfNestedList : List Value -> List Value
flattenIfNestedList jsonValues =
    case traverseMaybe asList jsonValues of
        Just nestedJsonList ->
            List.concat nestedJsonList

        Nothing ->
            jsonValues


getValueAt : Value -> String -> Result Error Value
getValueAt json key =
    -- Attempts to retrieve the value at the specified key in a JSON object
    json
        |> decodeValue (Json.Decode.field key Json.Decode.value)
        |> Result.mapError JsonDecodingError
