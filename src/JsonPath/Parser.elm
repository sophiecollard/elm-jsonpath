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
        , indices
        , keys
        ]


wildcard : Parser Selector
wildcard =
    succeed Wildcard
        |. symbol "*"


indices : Parser Selector
indices =
    succeed Indices
        |= int
        |= oneOf
            [ sequence
                { start = ","
                , separator = ","
                , end = ""
                , item = int
                , spaces = spaces
                , trailing = Forbidden
                }
            , succeed []
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
