module JsonPath.Error exposing
    ( Error(..), Cursor, CursorOp(..)
    , toPath
    )

{-|


# Type and Constructors

@docs Error, Cursor, CursorOp


# Functions

@docs toPath

-}

import Parser


{-| Describes an error encountered while attempting to extract the JSON at a given path.
-}
type Error
    = PathParsingError (List Parser.DeadEnd)
    | IndexNotFound Cursor Int
    | KeyNotFound Cursor String
    | NotAJsonArray Cursor
    | NotAJsonArrayNorAnObject Cursor


{-| A `Cursor` is used to facilitate debugging by keeping track of the location an `Error` was encountered.
It is made up of a list of operations (see `CursorOp`).
-}
type alias Cursor =
    List CursorOp


{-| A `Cursor` operation.
-}
type CursorOp
    = DownIndex Int
    | DownKey String


{-| Returns the path expression for a given `Cursor`.
-}
toPath : Cursor -> String
toPath cursor =
    let
        f : CursorOp -> String -> String
        f op acc =
            case op of
                DownIndex index ->
                    "." ++ String.fromInt index ++ acc

                DownKey key ->
                    "." ++ key ++ acc
    in
    cursor
        |> List.foldl f ""
        |> (++) "$"
