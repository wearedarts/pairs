module Message exposing (Msg(..))

import Card.Data exposing (Card, Level)


type Msg
    = SelectedCardSet String
    | CardSetLoaded
    | SelectedLevel Level
    | PressedPlay
    | PressedStartAgain
    | PressedChooseAnother
    | ShuffledCards (List Card)
    | SelectedCard Card
    | PressedConfirmToast
