module Message exposing (Msg(..))

import Card.Data exposing (Card, Level)
import Toasty


type Msg
    = SelectedCardSet String
    | SelectedLevel Level
    | PressedPlay
    | PressedStartAgain
    | PressedChooseAnother
    | ShuffledCards (List Card)
    | SelectedCard Card
    | ShowSpeech (Toasty.Msg String)
