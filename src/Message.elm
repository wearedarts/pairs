module Message exposing (Msg(..))

import Card.Data exposing (Card)


type Msg
    = PressedPlay
    | PressedHelp
    | ShuffledCards (List Card)
    | SelectedCard Card
