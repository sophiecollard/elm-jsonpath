module JsonPath.Extractor exposing (extract, run)

import Array
import Json.Decode exposing (Value, decodeValue)
import Json.Encode
import JsonPath exposing (JsonPath, Selector(..))
import JsonPath.Parser exposing (jsonPath)
import Parser


run : String -> Value -> Maybe Value
run rawPath json =
    case Parser.run jsonPath rawPath of
        Ok path ->
            extract path json

        Err _ ->
            Nothing


extract : JsonPath -> Value -> Maybe Value
extract path json =
    -- FIXME Return a Result with a meaningful error instead of a Maybe
    case path of
        [] ->
            Just json

        Wildcard :: _ ->
            Nothing

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
                        |> Maybe.map (Json.Encode.list identity)

                Err _ ->
                    -- FIXME Return a helpful error
                    Nothing

        (Indices i _) :: remainingSegments ->
            -- FIXME Handle multiple indices
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    Maybe.andThen
                        (\remainingJson -> extract remainingSegments remainingJson)
                        (Array.get (toPositiveIndex i (Array.length array)) array)

                Err _ ->
                    -- FIXME Return a helpful error
                    Nothing

        (Keys k _) :: remainingSegments ->
            -- FIXME Handle multiple keys
            case decodeValue (Json.Decode.field k Json.Decode.value) json of
                Ok remainingJson ->
                    extract remainingSegments remainingJson

                Err _ ->
                    -- FIXME Return a helpful error
                    Nothing


toPositiveIndex : Int -> Int -> Int
toPositiveIndex i length =
    if i < 0 then
        length + i

    else
        i


traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f list =
    -- Inspired by the traverse method on https://typelevel.org/cats/api/cats/Traverse.html
    List.map f list |> sequence


sequence : List (Maybe a) -> Maybe (List a)
sequence list =
    -- Inspired by the sequence method on https://typelevel.org/cats/api/cats/Traverse.html
    let
        loop : List a -> List (Maybe a) -> Maybe (List a)
        loop acc rem =
            case rem of
                [] ->
                    Just (List.reverse acc)

                Nothing :: _ ->
                    Nothing

                (Just a) :: nextRem ->
                    loop (a :: acc) nextRem
    in
    loop [] list
