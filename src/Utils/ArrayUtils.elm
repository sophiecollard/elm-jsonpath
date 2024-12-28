module Utils.ArrayUtils exposing (..)

import Array exposing (Array)
import JsonPath.Error exposing (Cursor, Error(..))


getElementAt : Array a -> Cursor -> Int -> Result Error a
getElementAt array cursor i =
    array
        |> Array.get i
        |> Result.fromMaybe (IndexNotFound cursor i)
