module Theme exposing (baseFont, baseShadow, theme)

import Css exposing (..)


theme :
    { primary : Color
    , primaryLighten20 : Color
    , secondary : Color
    }
theme =
    { primary = hex "48E048"
    , primaryLighten20 = hex "7BFF7B"
    , secondary = hex "111"
    }


baseFont : Style
baseFont =
    Css.batch
        [ color theme.primary
        , fontFamilies [ qt "VT323", monospace.value ]
        , fontSize (px 18)
        , textShadow4 zero zero (px 4) theme.primaryLighten20
        ]


baseShadow : Style
baseShadow =
    boxShadow4 zero zero (px 4) theme.primaryLighten20
