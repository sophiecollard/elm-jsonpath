module JsonPath.Parser exposing (jsonPath, segment, selector)

import JsonPath exposing (JsonPath, Selector(..))
import Parser exposing ((|.), (|=), Parser, Trailing(..), chompIf, chompWhile, end, getChompedString, int, oneOf, sequence, spaces, succeed, symbol)


jsonPath : Parser JsonPath
jsonPath =
    succeed identity
        |. symbol "$"
        |= sequence
            { start = ""
            , separator = ""
            , end = ""
            , item = segment
            , spaces = spaces
            , trailing = Forbidden
            }
        |. end


segment : Parser Selector
segment =
    oneOf
        [ bracketSegment
        , dotSegment
        ]


bracketSegment : Parser Selector
bracketSegment =
    succeed identity
        |. symbol "["
        |= selector
        |. symbol "]"


dotSegment : Parser Selector
dotSegment =
    succeed identity
        |. symbol "."
        |= selector


selector : Parser Selector
selector =
    oneOf
        [ wildcard
        , sliceOrIndices
        , keys
        ]


wildcard : Parser Selector
wildcard =
    succeed Wildcard
        |. symbol "*"


sliceOrIndices : Parser Selector
sliceOrIndices =
    oneOf
        [ Parser.andThen sliceOrIndicesTail signedInt

        -- Use a default start value of 0 if unspecified
        , Parser.andThen sliceTail (oneOf [ signedInt, succeed 0 ])
        ]


sliceOrIndicesTail : Int -> Parser Selector
sliceOrIndicesTail startOrHead =
    oneOf
        [ sliceTail startOrHead
        , indicesTail startOrHead
        ]


sliceTail : Int -> Parser Selector
sliceTail start =
    succeed (\end step -> Slice { start = start, end = end, step = step })
        |. symbol ":"
        -- Use a default end value of Nothing if unspecified
        -- Note that we cannot use -1 here, else the last element would not be included
        |= oneOf
            [ Parser.map Just signedInt
            , succeed Nothing
            ]
        -- Use a default step value of 1 if unspecified
        |= oneOf
            [ succeed identity |. symbol ":" |= signedInt
            , succeed 1
            ]


indicesTail : Int -> Parser Selector
indicesTail head =
    succeed (\tail -> Indices head tail)
        |= oneOf
            [ sequence
                { start = ","
                , separator = ","
                , end = ""
                , item = signedInt
                , spaces = spaces
                , trailing = Forbidden
                }
            , succeed []
            ]


signedInt : Parser Int
signedInt =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= int
        , int
        ]


keys : Parser Selector
keys =
    succeed Keys
        |= key
        |= oneOf
            [ sequence
                { start = ","
                , separator = ","
                , end = ""
                , item = key
                , spaces = spaces
                , trailing = Forbidden
                }
            , succeed []
            ]


key : Parser String
key =
    -- TODO According to the specification, name selectors should be placed between '' brackets
    getChompedString <|
        succeed ()
            |. chompIf (\c -> Char.isAlpha c || c == '_' || c == '-')
            |. chompWhile (\c -> Char.isAlphaNum c || c == '_' || c == '-')
