module JsonPath.Extractor exposing (run)

{-| The `Extractor` module exposes a single `run` method which attempts to extract the JSON `Value` at the
specified path.

@docs run

-}

import Array
import Dict
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import JsonPath exposing (Cursor, CursorOp(..), Error(..), Path, Selector(..))
import JsonPath.Parser exposing (path)
import Parser
import Utils.ArrayUtils exposing (getElementAt, slice)
import Utils.JsonUtils exposing (flattenIfNestedList, getValueAt)
import Utils.ListUtils exposing (collectOkValues, traverseResult)


{-| Attempts to extract the JSON `Value` at the specified path.

    run "$.foo[0]" False (Json.Encode.object [ ( "foo", Json.Encode.list Json.Encode.string [ "bar", "baz" ] ) ]) == Ok (Json.Encode.string "bar")

    run "$.foo.bar" False (Json.Encode.object [ ( "foo", Json.Encode.list Json.Encode.string [ "bar", "baz" ] ) ]) == Err (KeyNotFound [ DownKey "foo" ] "bar")

-}
run : String -> Bool -> Value -> Result Error Value
run rawPath strict json =
    case Parser.run path rawPath of
        Ok path ->
            extract path strict [] json

        Err err ->
            Err (PathParsingError err)


extract : Path -> Bool -> Cursor -> Value -> Result Error Value
extract path strict cursor json =
    let
        traverseOrCollect : (a -> Result e b) -> List a -> Result e (List b)
        traverseOrCollect f list =
            if strict then
                list
                    |> traverseResult f

            else
                list
                    |> List.map f
                    |> collectOkValues
                    |> Ok
    in
    case path of
        [] ->
            Ok json

        Wildcard :: remainingSegments ->
            case ( decodeValue (Json.Decode.list Json.Decode.value) json, decodeValue (Json.Decode.dict Json.Decode.value) json ) of
                ( Ok list, _ ) ->
                    list
                        |> List.indexedMap Tuple.pair
                        |> traverseOrCollect (\( i, value ) -> extract remainingSegments strict (DownIndex i :: cursor) value)
                        |> Result.map flattenIfNestedList
                        |> Result.map (Json.Encode.list identity)

                ( _, Ok dict ) ->
                    dict
                        |> Dict.toList
                        |> traverseOrCollect (\( k, value ) -> extract remainingSegments strict (DownKey k :: cursor) value)
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
                        |> traverseOrCollect (\( i, value ) -> extract remainingSegments strict (DownIndex i :: cursor) value)
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
                        |> Result.andThen (extract remainingSegments strict (DownIndex index :: cursor))

                Err _ ->
                    Err (NotAJsonArray cursor)

        (Indices index indices) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    (index :: indices)
                        |> List.map (toPositiveIndex (Array.length array))
                        |> traverseResult (\i -> getElementAt array cursor i |> Result.map (Tuple.pair i))
                        |> Result.andThen (traverseOrCollect (\( i, value ) -> extract remainingSegments strict (DownIndex i :: cursor) value))
                        |> Result.map flattenIfNestedList
                        |> Result.map (Json.Encode.list identity)

                Err _ ->
                    Err (NotAJsonArray cursor)

        (Keys key []) :: remainingSegments ->
            key
                |> getValueAt json cursor
                |> Result.andThen (extract remainingSegments strict (DownKey key :: cursor))

        (Keys key keys) :: remainingSegments ->
            (key :: keys)
                |> traverseResult (\k -> getValueAt json cursor k |> Result.map (Tuple.pair k))
                |> Result.andThen (traverseOrCollect (\( k, value ) -> extract remainingSegments strict (DownKey k :: cursor) value))
                |> Result.map flattenIfNestedList
                |> Result.map (Json.Encode.list identity)


toPositiveIndex : Int -> Int -> Int
toPositiveIndex length i =
    if i < 0 then
        length + i

    else
        i
