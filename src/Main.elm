module Main exposing (main)

import App exposing (Model, Msg, init, subscriptions, update, view)
import Browser


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
