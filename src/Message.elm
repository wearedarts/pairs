module Message exposing (Msg(..))

import Artist exposing (Artist)
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
    | ArtistSpeaks (List Artist)
    | PressedConfirmToast
