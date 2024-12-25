module JsonPath.Parser exposing (path)

{-| The `Parser` module exposes a `Parser Path` instance which allows parsing a path from a `String`.

@docs path

-}

import JsonPath exposing (Path, Segment(..), Selector(..))
import Parser exposing ((|.), (|=), Parser, Trailing(..), chompIf, chompWhile, end, getChompedString, int, oneOf, sequence, spaces, succeed, symbol)


{-| Allows parsing a path from a `String`.
-}
path : Parser Path
path =
    succeed identity
        |= sequence
            { start = "$"
            , separator = ""
            , end = ""
            , item = segment
            , spaces = spaces
            , trailing = Forbidden
            }
        |. end


segment : Parser Segment
segment =
    oneOf
        [ descendantsBracketNotation
        , descendantsDotNotation
        , childrenBracketNotation
        , childrenDotNotation
        ]


descendantsBracketNotation : Parser Segment
descendantsBracketNotation =
    succeed Descendants
        |. symbol "..["
        |= selector
        |. symbol "]"


descendantsDotNotation : Parser Segment
descendantsDotNotation =
    succeed Descendants
        |. symbol ".."
        |= selector


childrenBracketNotation : Parser Segment
childrenBracketNotation =
    succeed Children
        |. symbol "["
        |= selector
        |. symbol "]"


childrenDotNotation : Parser Segment
childrenDotNotation =
    succeed Children
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
    succeed (\maybeEnd step -> Slice { start = start, maybeEnd = maybeEnd, step = step })
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
