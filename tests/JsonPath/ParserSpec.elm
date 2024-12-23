module JsonPath.ParserSpec exposing (..)

import Expect exposing (equal)
import JsonPath exposing (Selector(..))
import JsonPath.Parser exposing (jsonPath, selector)
import Parser exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "The Parser module's"
        [ describe "selector method"
            [ test "should parse a wildcard selector" <|
                \_ ->
                    equal (run selector "*") (Ok Wildcard)
            , test "should parse a slice selector with start, end and step values" <|
                \_ ->
                    equal (run selector "1:5:1") (Ok (Slice { start = 1, maybeEnd = Just 5, step = 1 }))
            , test "should parse a slice selector with start and end values only" <|
                \_ ->
                    equal (run selector "1:5") (Ok (Slice { start = 1, maybeEnd = Just 5, step = 1 }))
            , test "should parse a slice with start value only" <|
                \_ ->
                    equal (run selector "2:") (Ok (Slice { start = 2, maybeEnd = Nothing, step = 1 }))
            , test "should parse a slice with end value only" <|
                \_ ->
                    equal (run selector ":-2") (Ok (Slice { start = 0, maybeEnd = Just -2, step = 1 }))
            , test "should parse a slice without start, end nor step values" <|
                \_ ->
                    equal (run selector ":") (Ok (Slice { start = 0, maybeEnd = Nothing, step = 1 }))
            , test "should parse a selector with a single index" <|
                \_ ->
                    equal (run selector "1") (Ok (Indices 1 []))
            , test "should parse a selector with multiple indices" <|
                \_ ->
                    equal (run selector "2,3,4") (Ok (Indices 2 [ 3, 4 ]))
            , test "should parse a selector with negative indices" <|
                \_ ->
                    equal (run selector "-1,-2,-3") (Ok (Indices -1 [ -2, -3 ]))
            , test "should parse a selector with a single key" <|
                \_ ->
                    equal (run selector "foo") (Ok (Keys "foo" []))
            , test "should parse a selector with multiple keys" <|
                \_ ->
                    equal (run selector "foo_,bar0,baZ") (Ok (Keys "foo_" [ "bar0", "baZ" ]))
            ]
        , describe "jsonPath method"
            [ test "should parse a path with no segments" <|
                \_ ->
                    equal (run jsonPath "$") (Ok [])
            , test "should parse a path composed of a single segment using bracket notation" <|
                \_ ->
                    equal (run jsonPath "$[foo,bar,baz]") (Ok [ Keys "foo" [ "bar", "baz" ] ])
            , test "should parse a path composed of a single segment using dot notation" <|
                \_ ->
                    equal (run jsonPath "$.foo") (Ok [ Keys "foo" [] ])
            , test "should parse a path composed of multiple segments using bracket notation" <|
                \_ ->
                    equal (run jsonPath "$[foo][1,2,3][bar,baz][*]")
                        (Ok
                            [ Keys "foo" []
                            , Indices 1 [ 2, 3 ]
                            , Keys "bar" [ "baz" ]
                            , Wildcard
                            ]
                        )

            -- , test "should parse a path composed of multiple segments using dot notation" <|
            --     \_ ->
            --         equal (run jsonPath "$.foo.1.bar.*")
            --             (Ok
            --                 [ Keys "foo" []
            --                 , Indices 1 []
            --                 , Keys "bar" []
            --                 , Wildcard
            --                 ]
            --             )
            , test "should parse a path composed of multiple segments using mixed notations" <|
                \_ ->
                    equal (run jsonPath "$.foo[1].bar[*]")
                        (Ok
                            [ Keys "foo" []
                            , Indices 1 []
                            , Keys "bar" []
                            , Wildcard
                            ]
                        )
            ]
        ]
