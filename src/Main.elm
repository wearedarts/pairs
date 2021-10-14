module Main exposing (main)

import Browser
import Card exposing (Card, Msg(..), initCardSet, renderCardList)
import Html exposing (Html, div, h1, text)
import Html.Events exposing (..)


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
    { cards : List Card
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cards = initCardSet }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedCard card ->
            let
                newCardState =
                    List.map
                        (\aCard ->
                            if card == aCard then
                                showCard model aCard

                            else if
                                flippedCardsIsEven (shownCards model.cards)
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


shownCards : List Card -> List Card
shownCards allCards =
    List.filter (\aCard -> aCard.isFlipped) allCards


showCard : Model -> Card -> Card
showCard model card =
    if not card.isFlipped then
        { card | isFlipped = True }

    else
        card


hideCard : Model -> Card -> Card
hideCard model card =
    if card.isFlipped then
        { card | isFlipped = False }

    else
        card


flippedCardsIsEven : List Card -> Bool
flippedCardsIsEven flippedCards =
    List.length flippedCards
        /= 0
        && modBy 2 (List.length flippedCards)
        == 0


notMatched : List Card -> Card -> Bool
notMatched cards card =
    (List.filter (\aCard -> aCard.isFlipped) cards
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
        , renderCardList model.cards
        ]
