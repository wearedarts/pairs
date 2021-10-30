module Card.View exposing (renderCardList)

import Card.Data exposing (Card)
import Html exposing (Html, button, img, li, text, ul)
import Html.Attributes exposing (alt, class, classList, disabled, src)
import Html.Events exposing (onClick)
import List
import Message exposing (Msg(..))


type Value
    = Image
    | Text


stringToValueType : String -> Value
stringToValueType aString =
    if String.contains "card-images/" aString then
        Image

    else
        Text


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
