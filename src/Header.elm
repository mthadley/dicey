module Header exposing (view)

import Css exposing (..)
import Css.Media exposing (only, screen)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class)


splash : String
splash =
    """
  _____     __     ______     ______     __  __
/\\  __-.  /\\ \\   /\\  ___\\   /\\  ___\\   /\\ \\_\\ \\
\\ \\ \\/\\ \\ \\ \\ \\  \\ \\ \\____  \\ \\  __\\   \\ \\____ \\
 \\ \\____-  \\ \\_\\  \\ \\_____\\  \\ \\_____\\  \\/\\_____\\
  \\/____/   \\/_/   \\/_____/   \\/_____/   \\/_____/
"""


view : Html msg
view =
    styled header
        [ display inlineBlock
        , marginBottom (px 16)
        ]
        []
        [ styled h1
            [ fontSize (vw 2.5)
            , maxWidth (pct 100)
            , whiteSpace Css.pre
            , Css.Media.withMedia [ only screen [ Css.Media.minWidth (px 768) ] ]
                [ fontSize (px 16)
                ]
            ]
            []
            [ text splash ]
        , styled p
            [ textAlign end ]
            []
            [ text "Roll some dice!" ]
        ]
