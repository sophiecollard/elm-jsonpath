module JsonPath.Extractor exposing (extract, run)

import Array
import Json.Decode exposing (Value, decodeValue)
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

        (Indices i []) :: remainingSegments ->
            -- FIXME Handle multiple indices
            case decodeValue (Json.Decode.array Json.Decode.value) json of
                Ok array ->
                    Maybe.andThen
                        (\remainingJson -> extract remainingSegments remainingJson)
                        (Array.get i array)

                Err _ ->
                    -- FIXME Return a helpful error
                    Nothing

        (Keys k []) :: remainingSegments ->
            -- FIXME Handle multiple keys
            case decodeValue (Json.Decode.field k Json.Decode.value) json of
                Ok remainingJson ->
                    extract remainingSegments remainingJson

                Err _ ->
                    -- FIXME Return a helpful error
                    Nothing

        _ ->
            -- FIXME Remove once all selectors are covered
            Nothing
