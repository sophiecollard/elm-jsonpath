module Utils.JsonUtils exposing (..)

import Json.Decode exposing (Value, decodeValue)
import JsonPath exposing (Cursor, Error(..))
import Utils.ListUtils exposing (traverseMaybe)


{-| Attempts to decode a JSON value as a List
-}
asList : Value -> Maybe (List Value)
asList json =
    json
        |> decodeValue (Json.Decode.list Json.Decode.value)
        |> Result.toMaybe


{-| Flattens a JSON array if and only if ALL the elements are themselves arrays.

Eg: [[1, 2], [3, 4, 5]] will be flattened to [1, 2, 3, 4, 5], but [[1, 2], 3, 4, 5] will remain as-is.

-}
flattenIfNestedList : List Value -> List Value
flattenIfNestedList jsonValues =
    --
    case traverseMaybe asList jsonValues of
        Just nestedJsonList ->
            List.concat nestedJsonList

        Nothing ->
            jsonValues


{-| Flattens ANY element within a JSON array which is itself an array.

Eg: [[1, 2], 3, 4, 5] will be flattened to [1, 2, 3, 4, 5].

-}
flattenNestedLists : List Value -> List Value
flattenNestedLists values =
    let
        loop : List Value -> List Value -> List Value
        loop rem acc =
            case rem of
                [] ->
                    List.reverse acc

                head :: tail ->
                    case asList head of
                        Just list ->
                            list
                                |> List.reverse
                                |> (\l -> List.append l acc)
                                |> loop tail

                        Nothing ->
                            loop tail (head :: acc)
    in
    loop values []


{-| Attempts to retrieve the value at the specified key
-}
getValueAt : Value -> Cursor -> String -> Result Error Value
getValueAt json cursor key =
    json
        |> decodeValue (Json.Decode.field key Json.Decode.value)
        |> Result.mapError (\_ -> KeyNotFound cursor key)
