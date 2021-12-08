module Main exposing (main)

import Browser
import Browser.Navigation
import Card.Data exposing (Card, availableCardSets, decodeCardSet, initCardSet)
import Card.View exposing (renderCardList)
import Html exposing (Html, a, button, div, fieldset, footer, h1, h2, h3, header, img, input, label, legend, main_, p, span, text)
import Html.Attributes exposing (alt, checked, class, for, href, id, src, type_)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Message exposing (Msg(..))
import Random
import Random.List


type alias Flags =
    { cardJson : { pairsList : Decode.Value, title : String, help : String }
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
    , cardSetMeta : { title : String, help : String }
    , selectedCardSet : String
    , cardsTried : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { isPlaying = False
      , helpClosed = False
      , cards = initCardSet (decodeCardSet flags.cardJson.pairsList)
      , cardSetMeta = { title = flags.cardJson.title, help = flags.cardJson.help }
      , selectedCardSet = flags.filename
      , cardsTried = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedCardSet setName ->
            ( model, Browser.Navigation.load ("?set=" ++ setName) )

        PressedPlay ->
            ( { model | isPlaying = True, helpClosed = True }
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
                                showCard aCard

                            else if
                                cardsIsEven (revealedCards model.cards)
                                    && notMatched model.cards aCard
                            then
                                hideCard aCard

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


showCard : Card -> Card
showCard card =
    if not card.isRevealed then
        { card | isRevealed = True }

    else
        card


hideCard : Card -> Card
hideCard card =
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
subscriptions _ =
    Sub.none


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Pairs", body = [ view model ] }


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ header [ class "page-section" ]
            [ div [ class "container" ]
[ a [ href "https://darts-games.netlify.app" ]
                    [ img [ src "/the-point-logo-cream.svg", alt "the point" ] []
                    , span [] [ text "Games" ]
                    ]
                ]
            ]
        , main_ [ class "page-section" ]
            [ div [ class "container fill-height" ]
                [ h1 [] [ text "Find the pairs" ]
                , if not model.isPlaying then
                    fieldset [] (legend [] [ h2 [] [ text "Choose a set" ] ] :: renderCardSetRadios model.selectedCardSet)

                  else
                    text ""
                , if model.isPlaying then
                    renderScore model

                  else
                    text ""
                , renderGameArea model
                , renderHelp model
                ]
            ]
        , footer [ class "page-section" ]
            [ a [ href "https://wearedarts.org.uk" ] [ text "wearedarts.org.uk" ] ]
        ]


renderCardSetRadios : String -> List (Html Msg)
renderCardSetRadios selectedCardSet =
    List.map
        (\{ title, file } ->
            let
                titleId =
                    String.toLower (String.replace " " "" title)
            in
            [ input
                [ type_ "radio"
                , onClick (SelectedCardSet file)
                , checked (file == selectedCardSet)
                , id titleId
                ]
                []
            , label [ for titleId ] [ text title ]
            ]
        )
        availableCardSets
        |> List.concat


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
    if not model.isPlaying then
        button [ onClick PressedPlay, class "play" ] [ text "Shuffle & Play!" ]

    else
        renderCardList model.cards


renderHelp : Model -> Html Msg
renderHelp model =
    if model.helpClosed then
        button [ class "help", onClick PressedHelp, ariaExpanded "false" ] [ h2 [] [ text "How to play +" ] ]

    else
        div []
            [ button [ class "help", onClick PressedHelp, ariaExpanded "true" ] [ h2 [] [ text "How to play -" ] ]
            , if model.helpClosed then
                text ""

              else
                div [ class "help-text" ]
                    [ p [] [ text model.cardSetMeta.help ]
                    , p [] [ text "Why not challenge yourself to guess it right first time?" ]
                    ]
            ]
