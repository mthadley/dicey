module Header exposing (view)

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
    header []
        [ h1 [ class "header-ascii" ] [ text splash ]
        , p [ class "header-tagline" ] [ text "Roll some dice!" ]
        ]
