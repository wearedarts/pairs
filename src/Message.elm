module Message exposing (Msg(..))

import Card.Data exposing (Card)


type Msg
    = SelectedCardSet String
    | PressedPlay
    | PressedHelp
    | ShuffledCards (List Card)
    | SelectedCard Card
