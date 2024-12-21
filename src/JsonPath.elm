module JsonPath exposing (JsonPath, Selector(..))


type alias JsonPath =
    List Selector


type Selector
    = Wildcard
    | Indices Int (List Int)
    | Keys String (List String)
