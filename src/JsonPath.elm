module JsonPath exposing (JsonPath, Selector(..))


type alias JsonPath =
    List Selector


type Selector
    = Wildcard
    | Slice { start : Int, end : Maybe Int, step : Int }
    | Indices Int (List Int)
    | Keys String (List String)
