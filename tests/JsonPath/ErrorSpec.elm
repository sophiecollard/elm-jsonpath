module JsonPath.ErrorSpec exposing (..)

import Expect exposing (equal)
import JsonPath.Error exposing (CursorOp(..), toPath)
import Test exposing (..)


suite : Test
suite =
    describe "Given a Cursor, the toPath function should return the corresponding path expression"
        [ test "$" <|
            \_ ->
                equal (toPath []) "$"
        , test "$.store.book.0.isbn" <|
            \_ ->
                equal (toPath [ DownKey "isbn", DownIndex 0, DownKey "book", DownKey "store" ]) "$.store.book.0.isbn"
        ]
