module Main exposing (main)

import Browser
import Card exposing (Card, Msg(..), initCardSet, renderCardList)
import Html exposing (Html, a, button, div, footer, h1, h2, h3, header, img, main_, p, text)
import Html.Attributes exposing (class, href, src)
import Html.Events exposing (..)
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
    , cardsTried : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { isPlaying = False, cards = initCardSet, cardsTried = 0 }
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
            ( { model
                | cards = newCardState
                , cardsTried = model.cardsTried + 1
              }
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


matched : List Card -> Card -> Bool
matched cards card =
    (List.filter (\aCard -> aCard.isRevealed) cards
        |> List.filter (\aCard -> aCard.value == card.match)
        |> List.length
    )
        == 1


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
                    , if model.isPlaying then
                        renderScore model

                      else
                        text ""
                    , renderGameArea model
                    ]
                , div []
                    [ h2 [] [ text "How to play" ]
                    , p [] [ text "[cCc] instructions " ]
                    , p [] [ text "[cCc] and help" ]
                    ]
                ]
            ]
        , footer []
            [ a [ href "https://wearedarts.org.uk" ] [ text "wearedarts.org.uk" ] ]
        ]


renderScore : Model -> Html Msg
renderScore model =
    let
        turns : Int
        turns =
            model.cardsTried // 2

        pairs : Int
        pairs =
            pairsFound (revealedCards model.cards)
    in
    h3 []
        [ text
            ("You've taken "
                ++ turnsToString turns
                ++ " and found "
                ++ pairsToString pairs
                ++ ". "
                ++ (if turns > 0 then
                        successToString turns pairs

                    else
                        ""
                   )
            )
        ]


pairsFound : List Card -> Int
pairsFound cardsShowing =
    (cardsShowing
        |> List.filter (\card -> matched cardsShowing card)
        |> List.length
    )
        // 2


turnsToString : Int -> String
turnsToString turns =
    String.fromInt turns
        ++ (if turns == 1 then
                " turn"

            else
                " turns"
           )


pairsToString : Int -> String
pairsToString matches =
    String.fromInt matches
        ++ (if matches == 1 then
                " pair"

            else
                " pairs"
           )


successToString : Int -> Int -> String
successToString turnsTaken pairs =
    ((toFloat pairs / toFloat turnsTaken)
        * 100
        |> round
        |> String.fromInt
    )
        ++ "% Success"


renderGameArea : Model -> Html Msg
renderGameArea model =
    div [ class "game-area" ]
        [ if not model.isPlaying then
            button [ onClick PressedPlay ] [ text "Shuffle & Play!" ]

          else
            renderCardList model.cards
        ]
