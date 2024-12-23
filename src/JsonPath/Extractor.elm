module JsonPath.Extractor exposing (extract, run)

import Array
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import JsonPath exposing (Error(..), Path, Selector(..))
import JsonPath.Parser exposing (jsonPath)
import Parser
import Utils.ArrayUtils exposing (getElementAt, slice)
import Utils.JsonUtils exposing (flattenIfNestedList, getValueAt)
import Utils.ListUtils exposing (traverseResult)


run : String -> Value -> Result Error Value
run rawPath json =
    case Parser.run jsonPath rawPath of
        Ok path ->
            extract path json

        Err err ->
            Err (PathParsingError err)


extract : Path -> Value -> Result Error Value
extract path json =
    case path of
        [] ->
            Ok json

        Wildcard :: remainingSegments ->
            case decodeValue (Json.Decode.list Json.Decode.value) json of
                Ok list ->
                    list
                        |> traverseResult (extract remainingSegments)
                        |> Result.map (Json.Encode.list identity)

                Err err ->
                    Err (JsonDecodingError err)

        (Slice { start, maybeEnd, step }) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    maybeEnd
                        |> Maybe.withDefault (Array.length array)
                        |> (\end -> slice start end step array)
                        |> Array.toList
                        |> traverseResult (extract remainingSegments)
                        |> Result.map (Json.Encode.list identity)

                Err err ->
                    Err (JsonDecodingError err)

        (Indices i []) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    i
                        |> toPositiveIndex (Array.length array)
                        |> getElementAt array
                        |> Result.andThen (extract remainingSegments)

                Err err ->
                    Err (JsonDecodingError err)

        (Indices i is) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    (i :: is)
                        |> List.map (toPositiveIndex (Array.length array))
                        |> traverseResult (getElementAt array)
                        |> Result.andThen (traverseResult (extract remainingSegments))
                        |> Result.map flattenIfNestedList
                        |> Result.map (Json.Encode.list identity)

                Err err ->
                    Err (JsonDecodingError err)

        (Keys k []) :: remainingSegments ->
            k
                |> getValueAt json
                |> Result.andThen (extract remainingSegments)

        (Keys k ks) :: remainingSegments ->
            (k :: ks)
                |> traverseResult (getValueAt json)
                |> Result.andThen (traverseResult (extract remainingSegments))
                |> Result.map flattenIfNestedList
                |> Result.map (Json.Encode.list identity)


toPositiveIndex : Int -> Int -> Int
toPositiveIndex length i =
    if i < 0 then
        length + i

    else
        i
