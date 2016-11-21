module Util exposing (..)


average : List Int -> Float
average values =
    (toFloat <| List.sum values) / (toFloat <| List.length values)


mapMaybeInt : (Maybe Int -> msg) -> String -> msg
mapMaybeInt msg =
    String.toInt >> Result.toMaybe >> msg


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
