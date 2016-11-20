module Util exposing (..)


average : List Int -> Float
average values =
    (toFloat <| List.sum values) / (toFloat <| List.length values)


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Nothing ->
            False

        _ ->
            True


isNothing : Maybe a -> Bool
isNothing =
    not << isJust
