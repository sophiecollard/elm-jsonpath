module Utils.ListUtils exposing (..)


collectJustValues : List (Maybe a) -> List a
collectJustValues list =
    -- Returns all the Just values in a List of Maybes
    let
        f : Maybe a -> List a -> List a
        f maybeA acc =
            case maybeA of
                Just a ->
                    a :: acc

                Nothing ->
                    acc
    in
    List.foldl f [] list


collectOkValues : List (Result e a) -> List a
collectOkValues list =
    -- Collects all the Ok values in a List of Results
    let
        f : Result e a -> List a -> List a
        f result acc =
            case result of
                Ok a ->
                    a :: acc

                Err _ ->
                    acc
    in
    List.foldl f [] list


traverseMaybe : (a -> Maybe b) -> List a -> Maybe (List b)
traverseMaybe f list =
    -- Inspired by the traverse method on https://typelevel.org/cats/api/cats/Traverse.html
    List.map f list |> sequenceMaybe


traverseResult : (a -> Result e b) -> List a -> Result e (List b)
traverseResult f list =
    -- Inspired by the traverse method on https://typelevel.org/cats/api/cats/Traverse.html
    List.map f list |> sequenceResult


sequenceMaybe : List (Maybe a) -> Maybe (List a)
sequenceMaybe list =
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


sequenceResult : List (Result e a) -> Result e (List a)
sequenceResult list =
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
