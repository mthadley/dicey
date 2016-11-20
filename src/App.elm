module App exposing (Model, Msg, init, update, view, subscriptions)

import Dice
import Header
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Random
import Util exposing (average, isNothing)


-- MODEL


type alias Model =
    { rolls : Maybe (List Int)
    , diceCount : Maybe Int
    , diceSize : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing Nothing 6, Cmd.none )


diceSizes : List Int
diceSizes =
    [ 2, 4, 6, 8, 12, 20 ]



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
                    , onInput <| toInt ChangeDiceCount
                    ]
                    []
                ]
            , div [ Attr.class "form-group" ]
                [ label [] [ text "Number of sides?" ]
                , viewDiceSelector model.diceSize
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
                , viewDice model.diceSize rolls
                ]


viewDice : Int -> List Int -> Html Msg
viewDice size rolls =
    div [ Attr.class "dice-container" ] <|
        List.map (toString >> Dice.view size False) rolls


viewDiceSelector : Int -> Html Msg
viewDiceSelector selectedSize =
    let
        sizeSelector size =
            button
                [ Attr.class "size-selector"
                , Attr.type_ "button"
                , onClick <| ChangeSize size
                ]
                [ Dice.view size (selectedSize /= size) <| toString size ]
    in
        div [ Attr.class "dice-selector" ] <| List.map sizeSelector diceSizes


viewStats : List Int -> Html Msg
viewStats rolls =
    ul [ Attr.class "results-stats" ]
        [ li [] [ text <| "Average: " ++ (String.left 4 <| toString <| average rolls) ]
        , li [] [ viewStat "Largest: " <| List.maximum rolls ]
        , li [] [ viewStat "Smallest: " <| List.minimum rolls ]
        ]


viewStat : String -> Maybe number -> Html msg
viewStat label value =
    text <| label ++ (toString <| Maybe.withDefault 0 <| value)



-- UPDATE


type Msg
    = ChangeSize Int
    | ChangeDiceCount (Maybe Int)
    | Roll
    | RollResults (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDiceCount count ->
            { model | diceCount = count, rolls = Nothing } ! []

        ChangeSize size ->
            ( { model | diceSize = size, rolls = Nothing }, rollIfReady model )

        Roll ->
            ( model, rollIfReady model )

        RollResults values ->
            { model | rolls = Just values } ! []


rollIfReady : Model -> Cmd Msg
rollIfReady { diceSize, diceCount } =
    Maybe.map (\count -> rollDice diceSize count) diceCount
        |> Maybe.withDefault Cmd.none


rollDice : Int -> Int -> Cmd Msg
rollDice size count =
    Random.int 1 size
        |> Random.list count
        |> Random.generate RollResults


toInt : (Maybe Int -> msg) -> String -> msg
toInt msg =
    String.toInt >> Result.toMaybe >> msg



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
