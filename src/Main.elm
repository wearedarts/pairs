module Main exposing (main)

import Browser
import Card exposing (Card, Msg(..), initCardSet, renderCardList)
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Random
import Random.List


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = viewDocument
        }


type alias Model =
    { isPlaying : Bool
    , cards : List Card
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { isPlaying = False, cards = initCardSet }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressedPlay ->
            ( { model | isPlaying = True }
            , Random.generate ShuffledCards (Random.List.shuffle model.cards)
            )

        ShuffledCards shuffledCards ->
            ( { model | cards = shuffledCards }, Cmd.none )

        SelectedCard card ->
            let
                newCardState =
                    List.map
                        (\aCard ->
                            if card == aCard then
                                showCard model aCard

                            else if
                                cardsIsEven (revealedCards model.cards)
                                    && notMatched model.cards aCard
                            then
                                hideCard model aCard

                            else
                                aCard
                        )
                        model.cards
            in
            ( { model | cards = newCardState }
            , Cmd.none
            )


revealedCards : List Card -> List Card
revealedCards allCards =
    List.filter (\aCard -> aCard.isRevealed) allCards


showCard : Model -> Card -> Card
showCard model card =
    if not card.isRevealed then
        { card | isRevealed = True }

    else
        card


hideCard : Model -> Card -> Card
hideCard model card =
    if card.isRevealed then
        { card | isRevealed = False }

    else
        card


cardsIsEven : List Card -> Bool
cardsIsEven cards =
    List.length cards
        /= 0
        && modBy 2 (List.length cards)
        == 0


notMatched : List Card -> Card -> Bool
notMatched cards card =
    (List.filter (\aCard -> aCard.isRevealed) cards
        |> List.filter (\aCard -> aCard.value == card.match)
        |> List.length
    )
        == 0


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Pairs", body = [ view model ] }


view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [ text "Find the Pairs" ]
        , if not model.isPlaying then
            button [ onClick PressedPlay ] [ text "Shuffle & Play!" ]

          else
            renderCardList model.cards
        ]
