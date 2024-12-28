module JsonPath.ParserSpec exposing (..)

import Expect exposing (equal)
import JsonPath.Parser exposing (Segment(..), Selector(..), path)
import Parser exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "The Parser module's path method"
        [ describe "should parse the following path expressions"
            [ test "$" <|
                \_ ->
                    equal (run path "$") (Ok [])
            , test "$.*.foo.0" <|
                \_ ->
                    equal (run path "$.*.foo.0")
                        (Ok
                            [ Children Wildcard
                            , Children (Keys "foo" [])
                            , Children (Indices 0 [])
                            ]
                        )
            , test "$[foo,bar,baz][11,12,13][*][::-1]" <|
                \_ ->
                    equal (run path "$[foo,bar,baz][11,12,13][*][::-1]")
                        (Ok
                            [ Children (Keys "foo" [ "bar", "baz" ])
                            , Children (Indices 11 [ 12, 13 ])
                            , Children Wildcard
                            , Children (Slice { start = 0, maybeEnd = Nothing, step = -1 })
                            ]
                        )
            , test "$.foo[-4,-5,-6].bar.baz[*]" <|
                \_ ->
                    equal (run path "$.foo[-4,-5,-6].bar.baz[*]")
                        (Ok
                            [ Children (Keys "foo" [])
                            , Children (Indices -4 [ -5, -6 ])
                            , Children (Keys "bar" [])
                            , Children (Keys "baz" [])
                            , Children Wildcard
                            ]
                        )
            , test "$..foo" <|
                \_ ->
                    equal (run path "$..foo")
                        (Ok
                            [ Descendants (Keys "foo" [])
                            ]
                        )
            , test "$..[foo,bar,baz]" <|
                \_ ->
                    equal (run path "$..[foo,bar,baz]")
                        (Ok
                            [ Descendants (Keys "foo" [ "bar", "baz" ])
                            ]
                        )
            , test "$.foo[*]..baz[::-1]" <|
                \_ ->
                    equal (run path "$.foo[*]..baz[::-1]")
                        (Ok
                            [ Children (Keys "foo" [])
                            , Children Wildcard
                            , Descendants (Keys "baz" [])
                            , Children (Slice { start = 0, maybeEnd = Nothing, step = -1 })
                            ]
                        )
            ]
        ]
