module Card exposing (Card, Msg(..), decodeCardSet, initCardSet, renderCardList)

import Html exposing (Html, button, img, li, text, ul)
import Html.Attributes exposing (class, classList, disabled, src)
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


type Msg
    = PressedPlay
    | ShuffledCards (List Card)
    | SelectedCard Card


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
                            text card.value

                          else
                            img [ src "point-card-back.png" ] []
                        ]
                    ]
            )
            cards
        )
