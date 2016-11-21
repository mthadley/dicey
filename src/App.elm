module App exposing (Model, Msg, init, update, view, subscriptions)

import Dice
import Header
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Random
import Util exposing (average, intFromInput, isNothing)
import Select


-- MODEL


type alias Model =
    { rolls : Maybe (List Int)
    , diceCount : Maybe Int
    , diceSize : Int
    , filterDropdown : Select.Model Filter
    , filterValue : Maybe Int
    , selectedFilter : Filter
    }


type Filter
    = GreaterThan
    | LessThan
    | Equal


initialFilter : Select.Item Filter
initialFilter =
    ( "Greater Than", GreaterThan )


selectItems : List ( String, Filter )
selectItems =
    [ initialFilter
    , ( "Less Than", LessThan )
    , ( "Equal", Equal )
    ]


init : ( Model, Cmd Msg )
init =
    { rolls = Nothing
    , diceCount = Nothing
    , diceSize = 6
    , filterDropdown = Select.init selectItems initialFilter
    , filterValue = Nothing
    , selectedFilter = Tuple.second initialFilter
    }
        ! []


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
                    , onInput <| intFromInput ChangeDiceCount
                    ]
                    []
                ]
            , div [ Attr.class "form-group" ]
                [ label [] [ text "Number of sides?" ]
                , viewDiceSelector model.diceSize
                ]
            , div [ Attr.class "form-group" ]
                [ label [ Attr.for "filterValue" ] [ text "Filter?" ]
                , Html.map FilterDropdownMsg <| Select.view model.filterDropdown
                , input
                    [ Attr.id "filterValue"
                    , onInput <| intFromInput ChangeFilterValue
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
                , viewDice model rolls
                ]


viewDice : Model -> List Int -> Html Msg
viewDice { diceSize, selectedFilter, filterValue } rolls =
    let
        filterHelper value =
            Maybe.map ((getFilterF selectedFilter) value) filterValue
                |> Maybe.withDefault True

        viewHelper value =
            Dice.view diceSize (filterHelper value) <| toString value
    in
        div [ Attr.class "dice-container" ] <| List.map viewHelper rolls


viewDiceSelector : Int -> Html Msg
viewDiceSelector selectedSize =
    let
        sizeSelector size =
            button
                [ Attr.class "size-selector"
                , Attr.type_ "button"
                , onClick <| ChangeSize size
                ]
                [ Dice.view size (selectedSize == size) <| toString size ]
    in
        div [ Attr.class "dice-selector" ] <| List.map sizeSelector diceSizes


viewStats : List Int -> Html Msg
viewStats rolls =
    let
        viewStat label value =
            text <| label ++ (toString <| Maybe.withDefault 0 <| value)
    in
        ul [ Attr.class "results-stats" ]
            [ li [] [ text <| "Average: " ++ (String.left 4 <| toString <| average rolls) ]
            , li [] [ viewStat "Largest: " <| List.maximum rolls ]
            , li [] [ viewStat "Smallest: " <| List.minimum rolls ]
            ]



-- UPDATE


type Msg
    = ChangeSize Int
    | ChangeFilterValue (Maybe Int)
    | ChangeDiceCount (Maybe Int)
    | FilterDropdownMsg (Select.Msg Filter)
    | Roll
    | RollResults (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFilterValue value ->
            { model | filterValue = value } ! []

        ChangeDiceCount count ->
            { model | diceCount = count, rolls = Nothing } ! []

        ChangeSize size ->
            let
                newModel =
                    { model | diceSize = size, rolls = Nothing }
            in
                ( newModel, rollIfReady newModel )

        FilterDropdownMsg childMsg ->
            let
                ( newModel, selected ) =
                    Select.update childMsg model.filterDropdown
            in
                { model | filterDropdown = newModel, selectedFilter = selected } ! []

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


getFilterF : Filter -> (comparable -> comparable -> Bool)
getFilterF filter =
    case filter of
        GreaterThan ->
            (>)

        LessThan ->
            (<)

        Equal ->
            (==)



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
