module JsonPath.Extractor exposing (run)

{-| The `Extractor` module exposes a single `run` method which attempts to extract the JSON `Value` at the
specified path.

@docs run

-}

import Array
import Dict
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import JsonPath exposing (Cursor, CursorOp(..), Error(..), Path, Segment(..), Selector(..))
import JsonPath.Parser exposing (path)
import Parser
import Utils.ArrayUtils exposing (getElementAt)
import Utils.JsonUtils exposing (flattenIfNestedList, flattenNestedLists, getValueAt)
import Utils.ListUtils exposing (collectOkValues, slice, traverseResult)
import Utils.ResultUtils exposing (combine)


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

        (Children Wildcard) :: remainingSegments ->
            case ( decodeValue (Json.Decode.array Json.Decode.value) json, decodeValue (Json.Decode.dict Json.Decode.value) json ) of
                ( Ok array, _ ) ->
                    array
                        |> Array.toIndexedList
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

        (Children (Slice { start, maybeEnd, step })) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    let
                        end =
                            Maybe.withDefault (Array.length array) maybeEnd
                    in
                    array
                        |> Array.toIndexedList
                        |> slice start end step
                        |> traverseOrCollect (\( i, value ) -> extract remainingSegments strict (DownIndex i :: cursor) value)
                        |> Result.map flattenIfNestedList
                        |> Result.map (Json.Encode.list identity)

                Err _ ->
                    Err (NotAJsonArray cursor)

        (Children (Indices index [])) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    index
                        |> toPositiveIndex (Array.length array)
                        |> getElementAt array cursor
                        |> Result.andThen (extract remainingSegments strict (DownIndex index :: cursor))

                Err _ ->
                    Err (NotAJsonArray cursor)

        (Children (Indices index indices)) :: remainingSegments ->
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

        (Children (Keys key [])) :: remainingSegments ->
            key
                |> getValueAt json cursor
                |> Result.andThen (extract remainingSegments strict (DownKey key :: cursor))

        (Children (Keys key keys)) :: remainingSegments ->
            (key :: keys)
                |> traverseResult (\k -> getValueAt json cursor k |> Result.map (Tuple.pair k))
                |> Result.andThen (traverseOrCollect (\( k, value ) -> extract remainingSegments strict (DownKey k :: cursor) value))
                |> Result.map flattenIfNestedList
                |> Result.map (Json.Encode.list identity)

        (Descendants Wildcard) :: remainingSegments ->
            case ( decodeValue (Json.Decode.array Json.Decode.value) json, decodeValue (Json.Decode.dict Json.Decode.value) json ) of
                ( Ok array, _ ) ->
                    let
                        valuesAtCursor : Result Error (List Value)
                        valuesAtCursor =
                            array
                                |> Array.toIndexedList
                                |> traverseOrCollect (\( i, value ) -> extract remainingSegments strict (DownIndex i :: cursor) value)

                        downstreamValues : Result Error (List Value)
                        downstreamValues =
                            array
                                |> Array.toIndexedList
                                |> traverseOrCollect (\( i, value ) -> extract path strict (DownIndex i :: cursor) value)
                                |> Result.map flattenNestedLists
                    in
                    combine List.append valuesAtCursor downstreamValues
                        |> Result.map (Json.Encode.list identity)

                ( _, Ok dict ) ->
                    let
                        valuesAtCursor : Result Error (List Value)
                        valuesAtCursor =
                            dict
                                |> Dict.toList
                                |> traverseOrCollect (\( k, value ) -> extract remainingSegments strict (DownKey k :: cursor) value)

                        downstreamValues : Result Error (List Value)
                        downstreamValues =
                            dict
                                |> Dict.toList
                                |> traverseOrCollect (\( k, value ) -> extract path strict (DownKey k :: cursor) value)
                                |> Result.map flattenNestedLists
                    in
                    combine List.append valuesAtCursor downstreamValues
                        |> Result.map (Json.Encode.list identity)

                _ ->
                    Ok (Json.Encode.list identity [])

        (Descendants (Slice { start, maybeEnd, step })) :: remainingSegments ->
            case ( decodeValue (Json.Decode.array Json.Decode.value) json, decodeValue (Json.Decode.dict Json.Decode.value) json ) of
                ( Ok array, _ ) ->
                    let
                        end =
                            Maybe.withDefault (Array.length array) maybeEnd

                        valuesAtCursor : Result Error (List Value)
                        valuesAtCursor =
                            array
                                |> Array.toIndexedList
                                |> slice start end step
                                |> traverseOrCollect (\( i, value ) -> extract remainingSegments strict (DownIndex i :: cursor) value)
                                |> Result.map flattenNestedLists

                        downstreamValues : Result Error (List Value)
                        downstreamValues =
                            array
                                |> Array.toIndexedList
                                |> traverseOrCollect (\( i, value ) -> extract path strict (DownIndex i :: cursor) value)
                                |> Result.map flattenNestedLists
                    in
                    combine List.append valuesAtCursor downstreamValues
                        |> Result.map (Json.Encode.list identity)

                ( _, Ok dict ) ->
                    dict
                        |> Dict.toList
                        |> traverseOrCollect (\( k, value ) -> extract path strict (DownKey k :: cursor) value)
                        |> Result.map flattenNestedLists
                        |> Result.map (Json.Encode.list identity)

                _ ->
                    Ok (Json.Encode.list identity [])

        (Descendants (Indices index indices)) :: remainingSegments ->
            case ( decodeValue (Json.Decode.array Json.Decode.value) json, decodeValue (Json.Decode.dict Json.Decode.value) json ) of
                ( Ok array, _ ) ->
                    let
                        valuesAtCursor : Result Error (List Value)
                        valuesAtCursor =
                            (index :: indices)
                                |> List.filterMap (\i -> Array.get i array |> Maybe.map (Tuple.pair i))
                                |> traverseOrCollect (\( i, value ) -> extract remainingSegments strict (DownIndex i :: cursor) value)
                                |> Result.map flattenNestedLists

                        downstreamValues : Result Error (List Value)
                        downstreamValues =
                            array
                                |> Array.toIndexedList
                                |> traverseOrCollect (\( i, value ) -> extract path strict (DownIndex i :: cursor) value)
                                |> Result.map flattenNestedLists
                    in
                    combine List.append valuesAtCursor downstreamValues
                        |> Result.map (Json.Encode.list identity)

                ( _, Ok dict ) ->
                    dict
                        |> Dict.toList
                        |> traverseOrCollect (\( k, value ) -> extract path strict (DownKey k :: cursor) value)
                        |> Result.map flattenNestedLists
                        |> Result.map (Json.Encode.list identity)

                _ ->
                    Ok (Json.Encode.list identity [])

        (Descendants (Keys key keys)) :: remainingSegments ->
            case ( decodeValue (Json.Decode.array Json.Decode.value) json, decodeValue (Json.Decode.dict Json.Decode.value) json ) of
                ( Ok array, _ ) ->
                    array
                        |> Array.toIndexedList
                        |> traverseOrCollect (\( i, value ) -> extract path strict (DownIndex i :: cursor) value)
                        |> Result.map flattenNestedLists
                        |> Result.map (Json.Encode.list identity)

                ( _, Ok dict ) ->
                    let
                        valuesAtCursor : Result Error (List Value)
                        valuesAtCursor =
                            (key :: keys)
                                |> List.filterMap (\k -> Dict.get k dict |> Maybe.map (Tuple.pair k))
                                |> traverseOrCollect (\( k, value ) -> extract remainingSegments strict (DownKey k :: cursor) value)
                                |> Result.map flattenNestedLists

                        downstreamValues : Result Error (List Value)
                        downstreamValues =
                            dict
                                |> Dict.toList
                                |> traverseOrCollect (\( k, value ) -> extract path strict (DownKey k :: cursor) value)
                                |> Result.map flattenNestedLists
                    in
                    combine List.append valuesAtCursor downstreamValues
                        |> Result.map (Json.Encode.list identity)

                _ ->
                    Ok (Json.Encode.list identity [])


toPositiveIndex : Int -> Int -> Int
toPositiveIndex length i =
    if i < 0 then
        length + i

    else
        i
