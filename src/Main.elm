module Main exposing (main)

import Browser
import Html exposing (..)
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
    { }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { }
    , Cmd.none
    )


type Msg
    = DoAction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoAction ->
            ( model
            , Cmd.none
            )


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
        [ text "Pairs"
        , Html.button
            [ Html.Events.onClick DoAction
            ]
            [ text "Do action!" ]
        ]
