module Dice exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Int -> Bool -> String -> Html msg
view sides active value =
    let
        fontY =
            if sides == 4 then
                "65"
            else
                "50"
    in
        svg
            [ class <|
                "dice"
                    ++ (if active then
                            ""
                        else
                            " dice-filtered"
                       )
            , width "100"
            , height "100"
            , viewBox "0 0 100 100"
            ]
            [ shape sides
            , text_
                [ alignmentBaseline "central"
                , textAnchor "middle"
                , id "text"
                , x "50"
                , y fontY
                , fontSize "42"
                ]
                [ text value ]
            ]


shape : Int -> Svg msg
shape sides =
    case sides of
        4 ->
            triangle

        6 ->
            square

        8 ->
            diamond

        12 ->
            pentagon

        20 ->
            hexagon

        _ ->
            circle


basePolygon : String -> Svg msg
basePolygon pts =
    polygon
        [ fill "none"
        , strokeWidth "5"
        , points pts
        ]
        []


circle : Svg msg
circle =
    Svg.circle
        [ cx "50"
        , cy "50"
        , r "47"
        , fill "none"
        , strokeWidth "5"
        ]
        []


diamond : Svg msg
diamond =
    basePolygon "50,2 98,50 50,98 2,50"


hexagon : Svg msg
hexagon =
    basePolygon "50,2 98,33 98,66 50,98 2,66 2,33"


pentagon : Svg msg
pentagon =
    basePolygon "50,2 98,45 80,98 20,98 2,45"


triangle : Svg msg
triangle =
    basePolygon "50,2 98,98 2,98"


square : Svg msg
square =
    basePolygon "2,2 98,2 98,98 2,98"
