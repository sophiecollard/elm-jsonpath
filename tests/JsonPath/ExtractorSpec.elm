module JsonPath.ExtractorSpec exposing (..)

import Expect exposing (equal)
import Json.Decode exposing (Value)
import Json.Encode
import JsonPath exposing (CursorOp(..), Error(..))
import JsonPath.Extractor
import Test exposing (..)


suite : Test
suite =
    describe "The Extractor module's"
        [ describe "run and runStrict methods"
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
                , test "$.store[*]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store[*]" sampleJson)
                            (Ok
                                (Json.Encode.list identity
                                    [ bicycle
                                    , Json.Encode.list identity [ book0, book1, book2, book3 ]
                                    ]
                                )
                            )
                , test "$.store.book..[*]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book..[*]" sampleJson)
                            (Ok
                                (Json.Encode.list identity
                                    [ book0
                                    , book1
                                    , book2
                                    , book3
                                    , Json.Encode.string "Nigel Rees"
                                    , Json.Encode.string "reference"
                                    , Json.Encode.float 8.95
                                    , Json.Encode.string "Sayings of the Century"
                                    , Json.Encode.string "Evelyn Waugh"
                                    , Json.Encode.string "fiction"
                                    , Json.Encode.float 12.99
                                    , Json.Encode.string "Sword of Honour"
                                    , Json.Encode.string "Herman Melville"
                                    , Json.Encode.string "fiction"
                                    , Json.Encode.string "0-553-21311-3"
                                    , Json.Encode.float 8.99
                                    , Json.Encode.string "Moby Dick"
                                    , Json.Encode.string "J. R. R. Tolkien"
                                    , Json.Encode.string "fiction"
                                    , Json.Encode.string "0-395-19395-8"
                                    , Json.Encode.float 22.99
                                    , Json.Encode.string "The Lord of the Rings"
                                    ]
                                )
                            )
                , test "$.store..[*].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store..[*].author" sampleJson)
                            (Ok
                                (Json.Encode.list Json.Encode.string
                                    [ "Nigel Rees"
                                    , "Evelyn Waugh"
                                    , "Herman Melville"
                                    , "J. R. R. Tolkien"
                                    ]
                                )
                            )
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
                , test "$.store..[::-2].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store..[::-2].author" sampleJson)
                            (Ok (Json.Encode.list Json.Encode.string [ "Herman Melville", "Nigel Rees" ]))
                ]
            , describe "should extract the elements in a path comprising an index selector"
                [ test "$.store.book[1]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[1]" sampleJson)
                            (Ok book1)
                , test "$.store.book[-2].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[-2].author" sampleJson)
                            (Ok (Json.Encode.string "Herman Melville"))
                , test "$.store.book[1,3]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[1,3]" sampleJson)
                            (Ok (Json.Encode.list identity [ book1, book3 ]))
                , test "$.store.book[-2,-1].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[-2,-1].title" sampleJson)
                            (Ok (Json.Encode.list Json.Encode.string [ "Moby Dick", "The Lord of the Rings" ]))
                , test "$.store..[0]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store..[0]" sampleJson)
                            (Ok (Json.Encode.list identity [ book0 ]))
                , test "$.store..[1,2,5].title" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store..[1,2,5].title" sampleJson)
                            (Ok (Json.Encode.list Json.Encode.string [ "Sword of Honour", "Moby Dick" ]))
                ]
            , describe "should extract the elements in a path comprising a key selector"
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
                , test "$.store..author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store..author" sampleJson)
                            (Ok
                                (Json.Encode.list Json.Encode.string
                                    [ "Nigel Rees"
                                    , "Evelyn Waugh"
                                    , "Herman Melville"
                                    , "J. R. R. Tolkien"
                                    ]
                                )
                            )
                , test "$.store..price" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store..price" sampleJson)
                            (Ok
                                (Json.Encode.list Json.Encode.float
                                    [ 19.95
                                    , 8.95
                                    , 12.99
                                    , 8.99
                                    , 22.99
                                    ]
                                )
                            )
                , test "$.store..[title,isbn]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store..[title,isbn]" sampleJson)
                            (Ok
                                (Json.Encode.list Json.Encode.string
                                    [ "Sayings of the Century"
                                    , "Sword of Honour"
                                    , "Moby Dick"
                                    , "0-553-21311-3"
                                    , "The Lord of the Rings"
                                    , "0-395-19395-8"
                                    ]
                                )
                            )
                ]
            , describe "should correctly report an 'index not found' error"
                [ test "$.store.book[5].author" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[5].author" sampleJson)
                            (Err (IndexNotFound [ DownKey "book", DownKey "store" ] 5))
                ]
            , describe "should correctly report a 'key not found' error"
                [ test "$.store.pet[*]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.pet[*]" sampleJson)
                            (Err (KeyNotFound [ DownKey "store" ] "pet"))
                , test "$.store.book[0][author,publisher]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[0][author,publisher]" sampleJson)
                            (Err (KeyNotFound [ DownIndex 0, DownKey "book", DownKey "store" ] "publisher"))
                ]
            , describe "should correctly report a 'not a JSON array' error"
                [ test "$.store[1]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store[1]" sampleJson)
                            (Err (NotAJsonArray [ DownKey "store" ]))
                ]
            , describe "should correctly report a 'not a JSON array nor an object' error"
                [ test "$.store.book[1].author[*]" <|
                    \_ ->
                        equal (JsonPath.Extractor.run "$.store.book[1].author[*]" sampleJson)
                            (Err (NotAJsonArrayNorAnObject [ DownKey "author", DownIndex 1, DownKey "book", DownKey "store" ]))
                ]
            , describe "should raise an error when only one result could be returned and the specified index/key is not found"
                [ describe "$.store.pet"
                    [ test "run" <|
                        \_ ->
                            equal (JsonPath.Extractor.run "$.store.pet" sampleJson)
                                (Err (KeyNotFound [ DownKey "store" ] "pet"))
                    , test "runStrict" <|
                        \_ ->
                            equal (JsonPath.Extractor.runStrict "$.store.pet" sampleJson)
                                (Err (KeyNotFound [ DownKey "store" ] "pet"))
                    ]
                , describe "$.store.book[5]"
                    [ test "run" <|
                        \_ ->
                            equal (JsonPath.Extractor.run "$.store.book[5]" sampleJson)
                                (Err (IndexNotFound [ DownKey "book", DownKey "store" ] 5))
                    , test "runStrict" <|
                        \_ ->
                            equal (JsonPath.Extractor.runStrict "$.store.book[5]" sampleJson)
                                (Err (IndexNotFound [ DownKey "book", DownKey "store" ] 5))
                    ]
                ]
            , describe "should ignore missing indices and keys (unless using strict mode) when more than one result could be returned"
                [ describe "$.store.book[:].isbn"
                    [ test "run" <|
                        \_ ->
                            equal (JsonPath.Extractor.run "$.store.book[:].isbn" sampleJson)
                                (Ok (Json.Encode.list Json.Encode.string [ "0-553-21311-3", "0-395-19395-8" ]))
                    , test "runStrict" <|
                        \_ ->
                            equal (JsonPath.Extractor.runStrict "$.store.book[:].isbn" sampleJson)
                                (Err (KeyNotFound [ DownIndex 0, DownKey "book", DownKey "store" ] "isbn"))
                    ]
                , describe "$.store[*][0]"
                    [ test "run" <|
                        \_ ->
                            equal (JsonPath.Extractor.run "$.store[*][0]" sampleJson)
                                (Ok (Json.Encode.list identity [ book0 ]))
                    , test "runStrict" <|
                        \_ ->
                            equal (JsonPath.Extractor.runStrict "$.store[*][0]" sampleJson)
                                (Err (NotAJsonArray [ DownKey "bicycle", DownKey "store" ]))
                    ]
                , describe "$.store[*][5]"
                    [ test "run" <|
                        \_ ->
                            equal (JsonPath.Extractor.run "$.store[*][5]" sampleJson)
                                (Ok (Json.Encode.list identity []))
                    , test "runStrict" <|
                        \_ ->
                            equal (JsonPath.Extractor.runStrict "$.store[*][5]" sampleJson)
                                (Err (NotAJsonArray [ DownKey "bicycle", DownKey "store" ]))
                    ]
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
