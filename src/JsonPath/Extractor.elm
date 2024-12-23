module JsonPath.Extractor exposing (extract, run)

import Array exposing (Array)
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import JsonPath exposing (Error(..), JsonPath, Selector(..))
import JsonPath.Parser exposing (jsonPath)
import Parser


run : String -> Value -> Result Error Value
run rawPath json =
    case Parser.run jsonPath rawPath of
        Ok path ->
            extract path json

        Err err ->
            Err (PathParsingError err)


extract : JsonPath -> Value -> Result Error Value
extract path json =
    case path of
        [] ->
            Ok json

        Wildcard :: remainingSegments ->
            case decodeValue (Json.Decode.list Json.Decode.value) json of
                Ok list ->
                    list
                        |> traverse (extract remainingSegments)
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
                        |> traverse (extract remainingSegments)
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
                        |> traverse (getElementAt array)
                        |> Result.andThen (traverse (extract remainingSegments))
                        |> Result.map (Json.Encode.list identity)

                Err err ->
                    Err (JsonDecodingError err)

        (Keys k []) :: remainingSegments ->
            k
                |> getValueAt json
                |> Result.andThen (extract remainingSegments)

        (Keys k ks) :: remainingSegments ->
            -- FIXME Don't forget to flatten the output if necessary
            (k :: ks)
                |> traverse (getValueAt json)
                |> Result.andThen (traverse (extract remainingSegments))
                |> Result.map (Json.Encode.list identity)


toPositiveIndex : Int -> Int -> Int
toPositiveIndex length i =
    if i < 0 then
        length + i

    else
        i


getElementAt : Array a -> Int -> Result Error a
getElementAt array i =
    array
        |> Array.get i
        |> Result.fromMaybe (IndexNotFound i)


getValueAt : Value -> String -> Result Error Value
getValueAt json key =
    json
        |> decodeValue (Json.Decode.field key Json.Decode.value)
        |> Result.mapError JsonDecodingError


slice : Int -> Int -> Int -> Array a -> Array a
slice start end step array =
    let
        posStep =
            if step < 0 then
                -1 * step

            else
                step

        enforceStep : Int -> a -> Maybe a
        enforceStep i a =
            if modBy posStep i == 0 then
                Just a

            else
                Nothing

        flatten : Maybe a -> Array a -> Array a
        flatten maybeA tail =
            case maybeA of
                Just a ->
                    [ a ]
                        |> Array.fromList
                        |> (\head -> Array.append head tail)

                Nothing ->
                    tail

        maybeReverse : Array a -> Array a
        maybeReverse array_ =
            -- A negative step value requires reversing the sliced values
            if step < 0 then
                array_
                    |> Array.toList
                    |> List.reverse
                    |> Array.fromList

            else
                array_
    in
    array
        |> Array.indexedMap enforceStep
        |> Array.slice start end
        |> Array.foldr flatten Array.empty
        |> maybeReverse


traverse : (a -> Result e b) -> List a -> Result e (List b)
traverse f list =
    -- Inspired by the traverse method on https://typelevel.org/cats/api/cats/Traverse.html
    List.map f list |> sequence


sequence : List (Result e a) -> Result e (List a)
sequence list =
    -- Inspired by the sequence method on https://typelevel.org/cats/api/cats/Traverse.html
    let
        loop : List a -> List (Result e a) -> Result e (List a)
        loop acc rem =
            case rem of
                [] ->
                    Ok (List.reverse acc)

                (Err e) :: _ ->
                    Err e

                (Ok a) :: nextRem ->
                    loop (a :: acc) nextRem
    in
    loop [] list
