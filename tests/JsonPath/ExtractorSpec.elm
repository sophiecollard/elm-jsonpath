module JsonPath.ExtractorSpec exposing (..)

import Expect exposing (equal)
import Json.Decode exposing (Value)
import Json.Encode
import JsonPath.Extractor
import Test exposing (..)


suite : Test
suite =
    describe "The Extractor module's"
        [ describe "run method"
            [ test "should extract the root element" <|
                \_ ->
                    equal (JsonPath.Extractor.run "$" sampleJson) (Ok sampleJson)
            , describe "should extract the elements in a path comprising a wildcard selector"
                [ test "$.store.book[*]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[*]" sampleJson)
                            (Ok (Json.Encode.list identity [ book0, book1, book2, book3 ]))
                , test "$.store.book[*].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[*].author" sampleJson)
                            (Ok (Json.Encode.list Json.Encode.string [ "Nigel Rees", "Evelyn Waugh", "Herman Melville", "J. R. R. Tolkien" ]))
                ]
            , describe "should extract the elements in a path comprising a slice selector"
                [ test "$.store.book[1:3]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[1:3]" sampleJson)
                            (Ok (Json.Encode.list identity [ book1, book2 ]))
                , test "$.store.book[2:]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[2:]" sampleJson)
                            (Ok (Json.Encode.list identity [ book2, book3 ]))
                , test "$.store.book[-3:]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[-3:]" sampleJson)
                            (Ok (Json.Encode.list identity [ book1, book2, book3 ]))
                , test "$.store.book[:].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[:].author" sampleJson)
                            (Ok
                                (Json.Encode.list Json.Encode.string
                                    [ "Nigel Rees"
                                    , "Evelyn Waugh"
                                    , "Herman Melville"
                                    , "J. R. R. Tolkien"
                                    ]
                                )
                            )
                , test "$.store.book[0:4:2]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[0:4:2]" sampleJson)
                            (Ok (Json.Encode.list identity [ book0, book2 ]))
                , test "$.store.book[::3]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[::3]" sampleJson)
                            (Ok (Json.Encode.list identity [ book0, book3 ]))
                , test "$.store.book[::-1]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[::-1]" sampleJson)
                            (Ok (Json.Encode.list identity [ book3, book2, book1, book0 ]))
                , test "$.store.book[::-2].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[::-2].author" sampleJson)
                            (Ok (Json.Encode.list Json.Encode.string [ "Herman Melville", "Nigel Rees" ]))
                ]
            , describe "should extract the elements in a path comprising an index selector with a single value"
                [ test "$.store.book[1]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[1]" sampleJson)
                            (Ok book1)
                , test "$.store.book[-2].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[-2].author" sampleJson)
                            (Ok (Json.Encode.string "Herman Melville"))
                ]
            , describe "should extract the elements in a path comprising an index selector with multiple values"
                [ test "$.store.book[1,3]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[1,3]" sampleJson)
                            (Ok (Json.Encode.list identity [ book1, book3 ]))
                , test "$.store.book[-2,-1].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[-2,-1].title" sampleJson)
                            (Ok (Json.Encode.list Json.Encode.string [ "Moby Dick", "The Lord of the Rings" ]))
                ]
            , describe "should extract the elements in a path comprising a key selector with multiple values"
                [ test "$.store.book[1][author,title]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[1][author,title]" sampleJson)
                            (Ok (Json.Encode.list Json.Encode.string [ "Evelyn Waugh", "Sword of Honour" ]))
                , test "$.store.book[-2,-1][author,title]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[-2,-1][author,title]" sampleJson)
                            (Ok
                                (Json.Encode.list Json.Encode.string
                                    [ "Herman Melville"
                                    , "Moby Dick"
                                    , "J. R. R. Tolkien"
                                    , "The Lord of the Rings"
                                    ]
                                )
                            )
                ]
            ]
        ]


sampleJson : Value
sampleJson =
    Json.Encode.object
        [ ( "store"
          , Json.Encode.object
                [ ( "book", Json.Encode.list identity [ book0, book1, book2, book3 ] )
                , ( "bicycle", bicycle )
                ]
          )
        ]


book0 : Value
book0 =
    Json.Encode.object
        [ ( "category", Json.Encode.string "reference" )
        , ( "author", Json.Encode.string "Nigel Rees" )
        , ( "title", Json.Encode.string "Sayings of the Century" )
        , ( "price", Json.Encode.float 8.95 )
        ]


book1 : Value
book1 =
    Json.Encode.object
        [ ( "category", Json.Encode.string "fiction" )
        , ( "author", Json.Encode.string "Evelyn Waugh" )
        , ( "title", Json.Encode.string "Sword of Honour" )
        , ( "price", Json.Encode.float 12.99 )
        ]


book2 : Value
book2 =
    Json.Encode.object
        [ ( "category", Json.Encode.string "fiction" )
        , ( "author", Json.Encode.string "Herman Melville" )
        , ( "title", Json.Encode.string "Moby Dick" )
        , ( "isbn", Json.Encode.string "0-553-21311-3" )
        , ( "price", Json.Encode.float 8.99 )
        ]


book3 : Value
book3 =
    Json.Encode.object
        [ ( "category", Json.Encode.string "fiction" )
        , ( "author", Json.Encode.string "J. R. R. Tolkien" )
        , ( "title", Json.Encode.string "The Lord of the Rings" )
        , ( "isbn", Json.Encode.string "0-395-19395-8" )
        , ( "price", Json.Encode.float 22.99 )
        ]


bicycle : Value
bicycle =
    Json.Encode.object
        [ ( "color", Json.Encode.string "red" )
        , ( "price", Json.Encode.float 19.95 )
        ]
