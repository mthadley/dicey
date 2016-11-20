module Util exposing (..)


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
