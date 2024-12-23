module Utils.ArrayUtils exposing (..)

import Array exposing (Array)
import JsonPath exposing (Error(..))


getElementAt : Array a -> Int -> Result Error a
getElementAt array i =
    array
        |> Array.get i
        |> Result.fromMaybe (IndexNotFound i)


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
