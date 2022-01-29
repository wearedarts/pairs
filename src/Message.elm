module Message exposing (Msg(..))

import Card.Data exposing (Card, Level)


type Msg
    = SelectedCardSet String
    | SelectedLevel Level
    | PressedPlay
    | PressedStartAgain
    | PressedChooseAnother
    | PressedHelp
    | ShuffledCards (List Card)
    | SelectedCard Card
