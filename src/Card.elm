module Card exposing (Card, Msg(..), availableCardSets, decodeCardSet, initCardSet, renderCardList)

import Html exposing (Html, button, img, li, text, ul)
import Html.Attributes exposing (alt, class, classList, disabled, src)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List
import Random
import Random.List


type alias Card =
    { value : String
    , match : String
    , isRevealed : Bool
    }


type Value
    = Image
    | Text


stringToValueType aString =
    if String.contains "card-images/" aString then
        Image

    else
        Text


type Msg
    = SelectedCardSet String
    | PressedPlay
    | ShuffledCards (List Card)
    | SelectedCard Card


type alias SetFileMeta =
    { title : String
    , file : String
    }


availableCardSets : List SetFileMeta
availableCardSets =
    -- WARNING these need to be kept in sync with the files in `src/json`
    [ { title = "Colours", file = "set1" }
    , { title = "Artists", file = "name-artist" }
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


renderCardList : List Card -> Html Msg
renderCardList cards =
    ul [ class "cards" ]
        (List.map
            (\card ->
                li [ class "card", classList [ ( "revealed", card.isRevealed ) ] ]
                    [ button
                        [ onClick (SelectedCard card)
                        , disabled card.isRevealed
                        ]
                        [ if card.isRevealed then
                            case stringToValueType card.value of
                                Image ->
                                    img [ src card.value, alt card.match ] []

                                Text ->
                                    text card.value

                          else
                            img [ src "point-card-back.png" ] []
                        ]
                    ]
            )
            cards
        )
