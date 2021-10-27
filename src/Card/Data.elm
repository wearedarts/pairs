module Card.Data exposing (Card)


type alias Card =
    { value : String
    , match : String
    , isRevealed : Bool
    }
