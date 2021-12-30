module Message exposing (Msg(..))

import Card.Data exposing (Card)


type Msg
    = SelectedCardSet String
    | PressedPlay
    | PressedStartAgain
    | PressedChooseAnother
    | PressedHelp
    | ShuffledCards (List Card)
    | SelectedCard Card
