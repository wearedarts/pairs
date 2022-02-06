module Main exposing (main)

import Browser
import Browser.Navigation
import Card.Data exposing (Card, Level(..), availableCardSets, decodeCardSet, initCardSet)
import Card.View exposing (renderCardList)
import Html exposing (Html, a, button, div, footer, h1, h2, header, img, li, main_, text, ul)
import Html.Attributes exposing (alt, class, classList, href, src, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Message exposing (Msg(..))
import Random
import Random.List
import Toasty


type alias Flags =
    { cardJson :
        { pairsList : Decode.Value
        , title : String
        , help : String
        }
    , filename : String
    }


type alias SoundEffect =
    { source : String
    , volume : Float
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
    , cards : List Card
    , cardSetMeta :
        { title : String
        , help : String
        }
    , selectedCardSet :
        { title : String
        , level : Maybe Level
        }
    , cardsTried : Int
    , toasties : Toasty.Stack String
    , playedSoundEffects : List SoundEffect
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        decodedCards =
            initCardSet (decodeCardSet flags.cardJson.pairsList)
    in
    ( { isPlaying = False
      , cards = decodedCards
      , cardSetMeta = { title = flags.cardJson.title, help = flags.cardJson.help }
      , selectedCardSet = { title = flags.filename, level = Nothing }
      , cardsTried = 0
      , toasties = Toasty.initialState
      , playedSoundEffects = []
      }
    , Random.generate ShuffledCards (Random.List.shuffle decodedCards)
    )
        |> Toasty.addPersistentToast toastyConfig ShowSpeech flags.cardJson.help


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedCardSet setName ->
            ( model
            , Browser.Navigation.load ("?set=" ++ setName)
            )

        SelectedLevel level ->
            update PressedPlay
                { model
                    | cards = getCardsByLevel level model.cards
                    , selectedCardSet = updateLevel level model.selectedCardSet
                }

        PressedPlay ->
            ( { model | isPlaying = True }
            , Random.generate ShuffledCards (Random.List.shuffle model.cards)
            )

        PressedStartAgain ->
            ( { model | isPlaying = True }
            , Random.generate ShuffledCards
                (Random.List.shuffle
                    (List.map
                        (\card -> { card | isRevealed = False })
                        model.cards
                    )
                )
            )

        PressedChooseAnother ->
            ( model, Browser.Navigation.load "/" )

        ShuffledCards shuffledCards ->
            ( { model | cards = shuffledCards }, Cmd.none )

        SelectedCard selectedCard ->
            ( { model
                | cards = updateCardState selectedCard model.cards
                , cardsTried = model.cardsTried + 1
                , playedSoundEffects = updateSoundEffects selectedCard model
              }
            , Cmd.none
            )

        ShowSpeech subMsg ->
            Toasty.update toastyConfig ShowSpeech subMsg model


toastyConfig : Toasty.Config Msg
toastyConfig =
    Toasty.config
        |> Toasty.transitionOutDuration 100
        |> Toasty.delay 8000
        |> Toasty.containerAttrs speechContainerStyles
        |> Toasty.itemAttrs speechBubbleStyles


speechContainerStyles : List (Html.Attribute Msg)
speechContainerStyles =
    [ class "toast container" ]


speechBubbleStyles : List (Html.Attribute Msg)
speechBubbleStyles =
    []


updateLevel :
    Level
    -> { title : String, level : Maybe Level }
    -> { title : String, level : Maybe Level }
updateLevel newLevel cardSet =
    { cardSet | level = Just newLevel }


maxEasyCount : Int
maxEasyCount =
    8


maxMediumCount : Int
maxMediumCount =
    12


getCardsByLevel : Level -> List Card -> List Card
getCardsByLevel level allCards =
    case level of
        Easy ->
            List.take maxEasyCount allCards

        Medium ->
            List.take maxMediumCount allCards

        Hard ->
            allCards


updateSoundEffects : Card -> Model -> List SoundEffect
updateSoundEffects selectedCard model =
    if modBy 2 model.cardsTried /= 0 then
        model.playedSoundEffects
            ++ newSoundEffects model.cards selectedCard

    else
        model.playedSoundEffects


updateCardState : Card -> List Card -> List Card
updateCardState selectedCard oldCardState =
    List.map
        (\aCard ->
            if selectedCard == aCard then
                showCard aCard

            else if
                cardsIsEven (revealedCards oldCardState)
                    && notMatched oldCardState aCard
            then
                hideCard aCard

            else
                aCard
        )
        oldCardState


newSoundEffects : List Card -> Card -> List SoundEffect
newSoundEffects oldCardState selectedCard =
    if matched oldCardState selectedCard then
        if
            List.length (revealedCards oldCardState)
                + 1
                == List.length oldCardState
        then
            [ { source = "win.wav", volume = 0.6 } ]

        else
            [ { source = "success.ogg", volume = 0.6 } ]

    else
        [ { source = "failure.ogg", volume = 0.6 } ]


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
                [ a [ href "/" ]
                    [ img [ src "/the-point-logo-cream.svg", alt "the point" ] []
                    ]
                , a [ href "https://darts-games.netlify.app/", class "more-games" ]
                    [ text "More Games" ]
                ]
            ]
        , main_ [ class "page-section" ]
            [ div [ class "container fill-height" ]
                [ h1 [] [ text "Find the pairs" ]
                , if not model.isPlaying then
                    div []
                        ([ h2 [ style "margin-top" "-25px" ]
                            [ if model.selectedCardSet.title == "empty" then
                                text "Choose a set"

                              else
                                text ("of " ++ model.cardSetMeta.title)
                            ]
                         , Toasty.view toastyConfig renderArtistSpeech ShowSpeech model.toasties
                         , ul [ class "card-set-choices" ]
                            (renderCardSetList model.selectedCardSet)
                         ]
                            ++ (if model.selectedCardSet.title == "empty" then
                                    []

                                else
                                    [ h2 [] [ text "Choose a level to play" ]
                                    , ul [ class "level-choices" ] (renderLevelList model.cards)
                                    ]
                               )
                        )

                  else
                    text ""
                , if model.isPlaying then
                    div [ class "text-center" ]
                        [ button
                            [ class "restart"
                            , onClick PressedStartAgain
                            ]
                            [ text "Shuffle and start again" ]
                        , button [ class "choose-again", onClick PressedChooseAnother ] [ text "Choose new cards" ]
                        , renderScore model
                        ]

                  else
                    text ""
                , renderGameArea model
                , div [] (List.map renderAudio model.playedSoundEffects)
                ]
            ]
        , footer [ class "page-section" ]
            [ a [ href "https://wearedarts.org.uk" ] [ text "wearedarts.org.uk" ] ]
        ]


renderLevelList : List Card -> List (Html Msg)
renderLevelList cards =
    [ li [] [ button [ onClick (SelectedLevel Easy) ] [ text "Easy" ] ]
    , if List.length cards > maxEasyCount then
        li [] [ button [ onClick (SelectedLevel Medium) ] [ text "Medium" ] ]

      else
        text ""
    , if List.length cards > maxMediumCount then
        li [] [ button [ onClick (SelectedLevel Hard) ] [ text "Hard" ] ]

      else
        text ""
    ]


renderCardSetList : { title : String, level : Maybe Level } -> List (Html Msg)
renderCardSetList selectedCardSetOptions =
    List.map
        (\{ title, iconSrc, file } ->
            li []
                [ button
                    [ classList [ ( "is-selected", file == selectedCardSetOptions.title ) ]
                    , onClick (SelectedCardSet file)
                    ]
                    [ img [ src iconSrc, alt "" ] [], div [] [ text title ] ]
                ]
        )
        availableCardSets


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
    div []
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
    if model.isPlaying then
        renderCardList model.cards

    else
        text ""


renderArtistSpeech : String -> Html Msg
renderArtistSpeech speech =
    div [ class "artist-speech container" ]
        [ img [ class "artist", alt "Avatar of Barbara Hepworth", src "card-images/barbara.svg" ] []
        , div [ class "speech right" ] [ text speech ]
        ]


renderAudio : SoundEffect -> Html Msg
renderAudio soundEffect =
    Html.audio
        [ Html.Attributes.controls False
        , Html.Attributes.autoplay True
        , Html.Attributes.property "volume" (Encode.string (String.fromFloat soundEffect.volume))
        ]
        [ Html.source [ src soundEffect.source ] [] ]
