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

        Wildcard :: _ ->
            Err NotImplemented

        (Slice { start, end, step }) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    let
                        slice =
                            -- FIXME Use step
                            end
                                |> Maybe.withDefault (Array.length array)
                                |> (\e -> Array.slice start e array)
                                |> Array.toList
                    in
                    slice
                        |> traverse (extract remainingSegments)
                        |> Result.map (Json.Encode.list identity)

                Err err ->
                    Err (JsonDecodingError err)

        (Indices i []) :: remainingSegments ->
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    Result.andThen
                        (extract remainingSegments)
                        (i |> toPositiveIndex (Array.length array) |> getElementAt array)

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
            case decodeValue (Json.Decode.field k Json.Decode.value) json of
                Ok remainingJson ->
                    extract remainingSegments remainingJson

                Err err ->
                    Err (JsonDecodingError err)

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
