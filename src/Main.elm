module Main exposing (main)

import App exposing (Model, Msg, init, update, view)
import Browser


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
