module Dice exposing (view)

import Css exposing (..)
import Css.Transitions
import Html.Styled as Html exposing (Html)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes as Attrs exposing (..)
import Theme


type alias Point =
    ( Int, Int )


view : Int -> Bool -> String -> Html msg
view sides active value =
    let
        ( shape, textY ) =
            getShape sides
    in
    styled svg
        [ property "backface-visibility" "hidden"
        , Css.fill Theme.theme.primary
        , Css.height (px 36)
        , margin4 zero (px 16) (px 16) zero
        , property "stroke" "#48E048"
        , Css.width (px 36)
        , Css.Transitions.transition
            [ Css.Transitions.opacity3 500 0 Css.Transitions.easeOut
            ]
        , if active then
            Css.batch []

          else
            Css.batch [ Css.opacity (num 0.5) ]
        ]
        [ class <|
            "dice"
                ++ (if active then
                        ""

                    else
                        " dice-filtered"
                   )
        , Attrs.width "100"
        , Attrs.height "100"
        , Attrs.viewBox "0 0 100 100"
        ]
        [ shape
        , Svg.text_
            [ textAnchor "middle"
            , x "50"
            , y textY
            , Attrs.fontSize "42"
            ]
            [ text value ]
        ]


getShape : Int -> ( Svg msg, String )
getShape sides =
    case sides of
        4 ->
            ( triangle, "75" )

        6 ->
            ( square, "60" )

        8 ->
            ( diamond, "60" )

        12 ->
            ( pentagon, "70" )

        20 ->
            ( hexagon, "60" )

        _ ->
            ( circle, "60" )


basePolygon : List Point -> Svg msg
basePolygon pts =
    let
        joinTuple ( x, y ) =
            String.fromInt x ++ "," ++ String.fromInt y
    in
    polygon
        [ Attrs.fill "none"
        , strokeWidth "5"
        , points <| String.join " " <| List.map joinTuple pts
        ]
        []


circle : Svg msg
circle =
    Svg.circle
        [ cx "50"
        , cy "50"
        , r "47"
        , Attrs.fill "none"
        , strokeWidth "5"
        ]
        []


diamond : Svg msg
diamond =
    basePolygon
        [ ( 50, 2 )
        , ( 98, 50 )
        , ( 50, 98 )
        , ( 2, 50 )
        ]


hexagon : Svg msg
hexagon =
    basePolygon
        [ ( 50, 2 )
        , ( 98, 33 )
        , ( 98, 66 )
        , ( 50, 98 )
        , ( 2, 66 )
        , ( 2, 33 )
        ]


pentagon : Svg msg
pentagon =
    basePolygon
        [ ( 50, 2 )
        , ( 98, 45 )
        , ( 80, 98 )
        , ( 20, 98 )
        , ( 2, 45 )
        ]


triangle : Svg msg
triangle =
    basePolygon
        [ ( 50, 2 )
        , ( 98, 98 )
        , ( 2, 98 )
        ]


square : Svg msg
square =
    basePolygon
        [ ( 2, 2 )
        , ( 98, 2 )
        , ( 98, 98 )
        , ( 2, 98 )
        ]
