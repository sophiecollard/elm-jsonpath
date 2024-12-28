module JsonPath.Error exposing (Error(..), Cursor, CursorOp(..))

{-|


# Type and Constructors

@docs Error, Cursor, CursorOp

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
