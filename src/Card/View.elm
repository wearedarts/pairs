module Card.View exposing (initCardSet, renderCardList)

import Card.Data exposing (Card)
import Html exposing (Html, button, img, li, text, ul)
import Html.Attributes exposing (class, classList, disabled, src)
import Html.Events exposing (onClick)
import List
import Message exposing (Msg(..))
import Random
import Random.List


cardPairs : List ( String, String )
cardPairs =
    [ ( "azure", "blue" )
    , ( "red", "crimson" )
    , ( "green", "emerald" )
    ]


initCardSet : List Card
initCardSet =
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
