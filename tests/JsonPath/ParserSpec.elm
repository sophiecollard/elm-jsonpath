module JsonPath.ParserSpec exposing (..)

import Expect exposing (equal)
import JsonPath exposing (Selector(..))
import JsonPath.Parser exposing (path)
import Parser exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "The Parser module's"
        [ describe "path method"
            [ test "should parse a path with no segments" <|
                \_ ->
                    equal (run path "$") (Ok [])
            , test "should parse a path composed of multiple segments using bracket notation" <|
                \_ ->
                    equal (run path "$[foo,bar,baz][1,2,3][*][::-1]")
                        (Ok
                            [ Keys "foo" [ "bar", "baz" ]
                            , Indices 1 [ 2, 3 ]
                            , Wildcard
                            , Slice { start = 0, maybeEnd = Nothing, step = -1 }
                            ]
                        )
            , test "should parse a path composed of multiple segments using mixed notations" <|
                \_ ->
                    equal (run path "$.foo[1,2,3].bar[*]")
                        (Ok
                            [ Keys "foo" []
                            , Indices 1 [ 2, 3 ]
                            , Keys "bar" []
                            , Wildcard
                            ]
                        )
            ]
        ]
