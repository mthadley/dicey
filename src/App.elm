module App exposing (Model, Msg, init, update, view, subscriptions)

import Dice
import Header
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Random
import Util exposing (isNothing)


-- MODEL


type alias Model =
    { rolls : Maybe (List Int)
    , diceCount : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Nothing, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ Attr.class "container" ]
        [ Header.view
        , viewMain model
        ]


viewButtonText : Maybe a -> Html msg
viewButtonText =
    Maybe.map (\_ -> "Reroll!") >> Maybe.withDefault "Roll!" >> text


viewMain : Model -> Html Msg
viewMain model =
    main_ []
        [ form [ Attr.class "dice-form", onSubmit Roll ]
            [ div [ Attr.class "form-group" ]
                [ label [ Attr.for "diceCount" ] [ text "How many dice?" ]
                , input
                    [ Attr.id "diceCount"
                    , onInput <| toInt DiceCount
                    ]
                    []
                ]
            , div []
                [ button
                    [ Attr.disabled <| isNothing model.diceCount
                    , Attr.type_ "submit"
                    ]
                    [ viewButtonText model.rolls ]
                ]
            ]
        , viewResults model
        ]


viewResults : Model -> Html Msg
viewResults model =
    case model.rolls of
        Nothing ->
            div [] []

        Just rolls ->
            section [ Attr.class "results-container" ]
                [ viewStats rolls
                , viewDice rolls
                ]


viewDice : List Int -> Html Msg
viewDice rolls =
    div [ Attr.class "dice-container" ] <| List.map Dice.view rolls


viewStats : List Int -> Html Msg
viewStats rolls =
    ul [ Attr.class "results-stats" ]
        [ li [] [ text <| "Average: " ++ (String.left 4 <| toString <| average rolls) ]
        , li [] [ viewStat "Max: " <| List.maximum rolls ]
        , li [] [ viewStat "Min: " <| List.minimum rolls ]
        ]


viewStat : String -> Maybe number -> Html msg
viewStat label value =
    text <| label ++ (toString <| Maybe.withDefault 0 <| value)



-- UPDATE


type Msg
    = Noop
    | DiceCount (Maybe Int)
    | Roll
    | RollResults (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        DiceCount count ->
            { model | diceCount = count, rolls = Nothing } ! []

        Roll ->
            case model.diceCount of
                Nothing ->
                    model ! []

                Just count ->
                    let
                        cmd =
                            Random.int 1 6
                                |> Random.list count
                                |> Random.generate RollResults
                    in
                        ( model, cmd )

        RollResults values ->
            { model | rolls = Just values } ! []


toInt : (Maybe Int -> msg) -> String -> msg
toInt msg =
    String.toInt >> Result.toMaybe >> msg


average : List Int -> Float
average values =
    (toFloat <| List.sum values) / (toFloat <| List.length values)



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
