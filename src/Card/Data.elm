module Card.Data exposing (Card, Level(..), availableCardSets, decodeCardSet, initCardSet)

import Json.Decode as Decode


type alias Card =
    { value : String
    , match : String
    , isRevealed : Bool
    }


type alias SetFileMeta =
    { title : String
    , iconSrc : String
    , file : String
    }


type Level
    = Easy
    | Medium
    | Hard


availableCardSets : List SetFileMeta
availableCardSets =
    -- WARNING title & file need to be in sync with the files in `src/cardData`
    [ { title = "Colours", iconSrc = "colours-set.png", file = "colour-shade" }
    , { title = "Artists", iconSrc = "artists-set.png", file = "name-artist" }
    , { title = "Artist tools", iconSrc = "tools-set.png", file = "tools" }
    ]


cardSetDecoder : Decode.Decoder (List ( String, String ))
cardSetDecoder =
    Decode.keyValuePairs Decode.string


decodeCardSet : Decode.Value -> List ( String, String )
decodeCardSet json =
    case Decode.decodeValue cardSetDecoder json of
        Ok cardSet ->
            cardSet

        Err _ ->
            []


initCardSet : List ( String, String ) -> List Card
initCardSet cardPairs =
    List.map
        (\( aValue, aMatch ) ->
            [ { value = aValue, match = aMatch, isRevealed = False }
            , { value = aMatch, match = aValue, isRevealed = False }
            ]
        )
        cardPairs
        |> List.concat
