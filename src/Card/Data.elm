module Card.Data exposing (Card, availableCardSets, decodeCardSet, initCardSet)

import Json.Decode as Decode


type alias Card =
    { value : String
    , match : String
    , isRevealed : Bool
    }


type alias SetFileMeta =
    { title : String
    , file : String
    }


availableCardSets : List SetFileMeta
availableCardSets =
    -- WARNING these need to be kept in sync with the files in `src/json`
    [ { title = "Colours", file = "set1" }
    , { title = "Artists", file = "name-artist" }
    , { title = "Artist Tools", file = "tools" }
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
            defaultCardPairs


defaultCardPairs : List ( String, String )
defaultCardPairs =
    [ ( "azure", "blue" )
    , ( "red", "crimson" )
    , ( "green", "emerald" )
    ]


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
