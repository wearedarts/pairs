module ArtistSpeech exposing (ProgressTrigger(..), getLevelHelp, getProgressSpeech, maxEasyCount, maxMediumCount)

import Random
import Random.List


getLevelHelp : Int -> String
getLevelHelp cardCount =
    if cardCount <= maxEasyCount then
        "Press Easy to play!"

    else if cardCount <= maxMediumCount then
        "Choose a level - Easy or Medium"

    else
        "Choose a level - Easy, Medium or Hard"


maxEasyCount : Int
maxEasyCount =
    8


maxMediumCount : Int
maxMediumCount =
    12


type ProgressTrigger
    = MatchedThree
    | MissedThreeInARow
    | MatchedAll
    | None


getProgressSpeech : ProgressTrigger -> List String
getProgressSpeech progressType =
    case progressType of
        MatchedThree ->
            [ "Brilliant! You are good at this"
            , "Amazing – you have a great memory"
            , "Fantastic! You are doing really well"
            , "You’re really paying attention – keep going!"
            , "Well done – you are nearly there"
            , "Impressive! Just a few to go"
            , "Great observation skills. Keep it up!"
            , "Super! You’re a pairs expert"
            ]

        MissedThreeInARow ->
            [ "You’ve got great focus – keep trying!"
            , "You are very motivated – keep it up!"
            , "Artists learn by trying again and again – don’t give up!"
            , "You are good at solving problems – keep going!"
            ]

        MatchedAll ->
            [ "Congratulations! Why not try a harder level or choose a different set of cards?"
            , "Congratulations! Have you tried a more challenging level?"
            , "Congratulations! You are becoming a pairs expert – why not try a different set of cards?"
            , "Congratulations! Now you are good at matching pairs, why not try colouring in a picture?"
            ]

        None ->
            [ "" ]
