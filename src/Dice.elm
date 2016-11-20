module Dice exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)


view : Int -> Html msg
view value =
    div [ class "dice" ] [ text <| toString value ]
