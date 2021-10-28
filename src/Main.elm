module Main exposing (main)

import Browser
import Browser.Navigation
import Card.Data exposing (Card, availableCardSets, decodeCardSet, initCardSet)
import Card.View exposing (renderCardList)
import Html exposing (Html, a, button, div, footer, h1, h2, header, img, input, label, main_, p, text)
import Html.Attributes exposing (checked, class, href, src, type_)
import Html.Events exposing (..)
import Json.Decode as Decode
import Message exposing (Msg(..))
import Random
import Random.List
import Url
import Url.Parser as Parser
import Url.Parser.Query as Query


type alias Flags =
    { cardJson : { pairsList : Decode.Value }
    , filename : String
    }


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
    , helpClosed : Bool
    , cards : List Card
    , selectedCardSet : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { isPlaying = False
      , helpClosed = True
      , cards = initCardSet (decodeCardSet flags.cardJson.pairsList)
      , selectedCardSet = flags.filename
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedCardSet setName ->
            ( model, Browser.Navigation.load ("?set=" ++ setName) )

        PressedPlay ->
            ( { model | isPlaying = True }
            , Random.generate ShuffledCards (Random.List.shuffle model.cards)
            )

        ShuffledCards shuffledCards ->
            ( { model | cards = shuffledCards }, Cmd.none )

        PressedHelp ->
            ( { model | helpClosed = not model.helpClosed }, Cmd.none )

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
    div [ class "page" ]
        [ header []
            [ div [ class "container" ]
                [ img [ src "/darts-logo-cream.svg" ] []
                ]
            ]
        , main_ []
            [ div [ class "container" ]
                [ div [ class "content" ]
                    [ h1 [] [ text "Find the pairs" ]
                    , if not model.isPlaying then
                        div [] (renderCardSetRadios model.selectedCardSet)

                      else
                        text ""
                    , renderGameArea model
                    ]
                , renderHelp model
                ]
            ]
        , footer []
            [ a [ href "https://wearedarts.org.uk" ] [ text "wearedarts.org.uk" ] ]
        ]


renderCardSetRadios : String -> List (Html Msg)
renderCardSetRadios selectedCardSet =
    List.map
        (\{ title, file } ->
            [ input
                [ type_ "radio"
                , onClick (SelectedCardSet file)
                , checked (file == selectedCardSet)
                ]
                []
            , label [] [ text title ]
            ]
        )
        availableCardSets
        |> List.concat


renderGameArea : Model -> Html Msg
renderGameArea model =
    div [ class "game-area" ]
        [ if not model.isPlaying then
            button [ onClick PressedPlay ] [ text "Shuffle & Play!" ]

          else
            renderCardList model.cards
        ]


renderHelp : Model -> Html Msg
renderHelp model =
    if model.isPlaying && model.helpClosed then
        button [ onClick PressedHelp ] [ text "How to play" ]

    else
        div []
            [ h2 []
                [ text "How to play "
                , if model.isPlaying then
                    button [ onClick PressedHelp ] [ text "Close how to play" ]

                  else
                    text ""
                ]
            , p [] [ text "[cCc] instructions " ]
            , p [] [ text "[cCc] and help" ]
            ]
