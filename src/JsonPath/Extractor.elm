module JsonPath.Extractor exposing (run)

import Array
import Dict
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import JsonPath exposing (Cursor, CursorOp(..), Error(..), Path, Selector(..))
import JsonPath.Parser exposing (jsonPath)
import Parser
import Utils.ArrayUtils exposing (getElementAt, slice)
import Utils.JsonUtils exposing (flattenIfNestedList, getValueAt)
import Utils.ListUtils exposing (traverseResult)


run : String -> Value -> Result Error Value
run rawPath json =
    case Parser.run jsonPath rawPath of
        Ok path ->
            extract path [] json

        Err err ->
            Err (PathParsingError err)


extract : Path -> Cursor -> Value -> Result Error Value
extract path cursor json =
    case path of
        [] ->
            Ok json

        Wildcard :: remainingSegments ->
            case ( decodeValue (Json.Decode.list Json.Decode.value) json, decodeValue (Json.Decode.dict Json.Decode.value) json ) of
                ( Ok list, _ ) ->
                    list
                        |> List.indexedMap Tuple.pair
                        |> traverseResult (\( i, value ) -> extract remainingSegments (DownIndex i :: cursor) value)
                        |> Result.map flattenIfNestedList
                        |> Result.map (Json.Encode.list identity)

                ( _, Ok dict ) ->
                    dict
                        |> Dict.toList
                        |> traverseResult (\( k, value ) -> extract remainingSegments (DownKey k :: cursor) value)
                        |> Result.map flattenIfNestedList
                        |> Result.map (Json.Encode.list identity)

                _ ->
                    Err (NotAJsonArrayNorAnObject cursor)

        (Slice { start, maybeEnd, step }) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    let
                        end =
                            Maybe.withDefault (Array.length array) maybeEnd
                    in
                    array
                        |> Array.indexedMap Tuple.pair
                        |> slice start end step
                        |> Array.toList
                        |> traverseResult (\( i, value ) -> extract remainingSegments (DownIndex i :: cursor) value)
                        |> Result.map flattenIfNestedList
                        |> Result.map (Json.Encode.list identity)

                Err _ ->
                    Err (NotAJsonArray cursor)

        (Indices index []) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    index
                        |> toPositiveIndex (Array.length array)
                        |> getElementAt array cursor
                        |> Result.andThen (extract remainingSegments (DownIndex index :: cursor))

                Err _ ->
                    Err (NotAJsonArray cursor)

        (Indices index indices) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    (index :: indices)
                        |> List.map (toPositiveIndex (Array.length array))
                        |> traverseResult (\i -> getElementAt array cursor i |> Result.map (Tuple.pair i))
                        |> Result.andThen (traverseResult (\( i, value ) -> extract remainingSegments (DownIndex i :: cursor) value))
                        |> Result.map flattenIfNestedList
                        |> Result.map (Json.Encode.list identity)

                Err _ ->
                    Err (NotAJsonArray cursor)

        (Keys key []) :: remainingSegments ->
            key
                |> getValueAt json cursor
                |> Result.andThen (extract remainingSegments (DownKey key :: cursor))

        (Keys key keys) :: remainingSegments ->
            (key :: keys)
                |> traverseResult (\k -> getValueAt json cursor k |> Result.map (Tuple.pair k))
                |> Result.andThen (traverseResult (\( k, value ) -> extract remainingSegments (DownKey k :: cursor) value))
                |> Result.map flattenIfNestedList
                |> Result.map (Json.Encode.list identity)


toPositiveIndex : Int -> Int -> Int
toPositiveIndex length i =
    if i < 0 then
        length + i

    else
        i
