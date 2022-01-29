module Message exposing (Msg(..))

import Card.Data exposing (Card)
import Toasty


type Msg
    = SelectedCardSet String
    | PressedPlay
    | PressedStartAgain
    | PressedChooseAnother
    | ShuffledCards (List Card)
    | SelectedCard Card
    | ShowSpeech (Toasty.Msg String)
