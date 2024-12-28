module Utils.ResultUtils exposing (..)


combine : (a -> b -> c) -> Result x a -> Result x b -> Result x c
combine f resultA resultB =
    Result.andThen (\a -> Result.map (\b -> f a b) resultB) resultA
