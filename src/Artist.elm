module Artist exposing (Artist, allArtists, artistToAlt, artistToSvgSrc)


type Artist
    = Andy
    | Barbara
    | Frida
    | Georgia
    | Vincent
    | Yinka


allArtists : List Artist
allArtists =
    [ Andy, Barbara, Frida, Georgia, Vincent, Yinka ]


artistToSvgSrc : Artist -> String
artistToSvgSrc artist =
    "/card-images/"
        ++ (case artist of
                Andy ->
                    "andy.svg"

                Barbara ->
                    "barbara.svg"

                Frida ->
                    "frida.svg"

                Georgia ->
                    "georgia.svg"

                Vincent ->
                    "vincent.svg"

                Yinka ->
                    "yinka.svg"
           )


artistToAlt : Artist -> String
artistToAlt artist =
    "Avatar of "
        ++ (case artist of
                Andy ->
                    "Andy Warhol"

                Barbara ->
                    "Barbara Hepworth"

                Frida ->
                    "Frida Kahlo"

                Georgia ->
                    "Georgia O'Keeffe"

                Vincent ->
                    "Vincent van Gogh"

                Yinka ->
                    "Yinka Shonibare"
           )
