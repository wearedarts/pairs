module Card exposing (Card, Msg(..), initCardSet, renderCardList)

import Html exposing (Html, button, li, text, ul)
import Html.Events exposing (onClick)
import List


type alias Card =
    { value : String
    , match : String
    , isRevealed : Bool
    }


type Msg
    = SelectedCard Card


cardPairs : List ( String, String )
cardPairs =
    [ ( "azure", "blue" ), ( "red", "crimson" ), ( "green", "emerald" ) ]


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
    ul []
        (List.map
            (\card ->
                li []
                    [ button [ onClick (SelectedCard card) ]
                        [ if card.isRevealed then
                            text card.value

                          else
                            text "BACK"
                        ]
                    ]
            )
            cards
        )
