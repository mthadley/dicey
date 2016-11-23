module App exposing (Model, Msg, init, update, view, subscriptions)

import Dice
import Header
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Random
import Util exposing (average, mapMaybeInt, isNothing, isEmpty)
import Select


-- MODEL


type alias Model =
    { rolls : Maybe (List Int)
    , diceCount : Maybe Int
    , diceSize : Int
    , diceCountInput : String
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
    , diceCountInput = ""
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
    Maybe.map (always "Reroll!") >> Maybe.withDefault "Roll!" >> text


viewMain : Model -> Html Msg
viewMain model =
    main_ []
        [ form [ Attr.class "dice-form", onSubmit Roll ]
            [ div [ Attr.class "form-group" ]
                [ label [ Attr.for "diceCount" ] [ text "How many dice?" ]
                , input
                    [ Attr.id "diceCount"
                    , onInput ChangeDiceCount
                    , Attr.value <|
                        Maybe.withDefault model.diceCountInput <|
                            Maybe.map toString model.diceCount
                    ]
                    []
                ]
            , div [ Attr.class "form-group" ]
                [ label [] [ text "Number of sides?" ]
                , viewDiceSelector model.diceSize
                ]
            , div [ Attr.class "form-group" ]
                [ label [ Attr.for "filterValue" ] [ text "Test?" ]
                , Html.map FilterDropdownMsg <| Select.view model.filterDropdown
                , input
                    [ Attr.id "filterValue"
                    , onInput <| mapMaybeInt ChangeFilterValue
                    ]
                    []
                ]
            , div [ Attr.class "btn-group" ]
                [ button
                    [ Attr.disabled <| isNothing model.diceCount
                    , Attr.type_ "submit"
                    ]
                    [ viewButtonText model.rolls ]
                , button
                    [ Attr.disabled <| filterBtnDisabled model
                    , Attr.type_ "button"
                    , onClick ApplyFilter
                    ]
                    [ text "Filter" ]
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
                , if List.isEmpty rolls then
                    em [] [ text "No More Dice!" ]
                  else
                    viewDice model rolls
                ]


viewDice : Model -> List Int -> Html Msg
viewDice { diceSize, selectedFilter, filterValue } rolls =
    let
        filterHelper value =
            Maybe.map (getFilterF selectedFilter value) filterValue
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
    = ApplyFilter
    | ChangeSize Int
    | ChangeFilterValue (Maybe Int)
    | ChangeDiceCount String
    | FilterDropdownMsg (Select.Msg Filter)
    | Roll
    | RollResults (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApplyFilter ->
            let
                filterHelper =
                    List.filter << (flip <| getFilterF model.selectedFilter)

                newRolls =
                    Maybe.map2 filterHelper model.filterValue model.rolls

                newDiceCount =
                    Maybe.map List.length newRolls
            in
                { model | rolls = newRolls, diceCount = newDiceCount } ! []

        ChangeFilterValue value ->
            { model | filterValue = value } ! []

        ChangeDiceCount count ->
            { model
                | diceCount = Result.toMaybe <| String.toInt count
                , diceCountInput = count
                , rolls = Nothing
            }
                ! []

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
    Maybe.map (rollDice diceSize) diceCount
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


filterBtnDisabled : Model -> Bool
filterBtnDisabled model =
    False



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
