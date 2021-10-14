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
                                doFlip model aCard

                            else
                                aCard
                        )
                        model.cards
            in
            ( { model | cards = newCardState }
            , Cmd.none
            )


doFlip : Model -> Card -> Card
doFlip model card =
    if not card.isFlipped then
        { card | isFlipped = True }

    else
        card


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
