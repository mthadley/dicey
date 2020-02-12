module App exposing (Model, Msg, init, update, view)

import Browser
import Css exposing (..)
import Css.Global
import Css.Transitions exposing (transition)
import Dice
import Header
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (..)
import Random
import Select
import Theme
import Util exposing (average, isNothing)



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
    | GreaterThanEq
    | LessThan
    | LessThanEq
    | Equal


initialFilter : Select.Item Filter
initialFilter =
    ( ">", GreaterThan )


selectItems : List ( String, Filter )
selectItems =
    [ initialFilter
    , ( "≥", GreaterThanEq )
    , ( "<", LessThan )
    , ( "≤", LessThanEq )
    , ( "=", Equal )
    ]


init : ( Model, Cmd Msg )
init =
    ( { rolls = Nothing
      , diceCount = Nothing
      , diceSize = 6
      , filterDropdown = Select.init selectItems initialFilter
      , filterValue = Nothing
      , selectedFilter = Tuple.second initialFilter
      }
    , Cmd.none
    )


diceSizes : List Int
diceSizes =
    [ 2, 4, 6, 8, 12, 20 ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Dicey: Roll some dice!"
    , body =
        [ Css.Global.global
            [ Css.Global.everything
                [ boxSizing borderBox
                ]
            , Css.Global.body
                [ Theme.baseFont
                , backgroundColor Theme.theme.secondary
                , color Theme.theme.primary
                , property "-webkit-tap-highlight-color" "#48E048"
                ]
            ]
        , styled div
            [ margin2 zero auto
            , maxWidth (px 768)
            ]
            []
            [ Header.view
            , viewMain model
            ]
        ]
            |> List.map toUnstyled
    }


viewButtonText : Maybe a -> Html msg
viewButtonText =
    Maybe.map (always "Reroll!") >> Maybe.withDefault "Roll!" >> text


viewMain : Model -> Html Msg
viewMain model =
    main_ []
        [ styled form
            [ marginBottom (px 16) ]
            [ onSubmit Roll
            ]
            [ viewFormGroup
                [ viewLabel [ Attr.for "diceCount" ] [ text "How many dice?" ]
                , viewInput
                    [ Attr.id "diceCount"
                    , Attr.type_ "number"
                    , onInput <| (String.toInt >> ChangeDiceCount)
                    , Attr.value <|
                        Maybe.withDefault "" <|
                            Maybe.map String.fromInt model.diceCount
                    ]
                    []
                ]
            , viewFormGroup
                [ viewLabel [] [ text "Number of sides?" ]
                , viewDiceSelector model.diceSize
                ]
            , viewFormGroup
                [ viewLabel
                    [ Attr.for "filterValue"
                    , Attr.title "In polish notation."
                    ]
                    [ text "Test?" ]
                , styled span
                    [ marginRight (px 16) ]
                    []
                    [ Select.view model.filterDropdown
                        |> Html.map FilterDropdownMsg
                    ]
                , viewInput
                    [ Attr.id "filterValue"
                    , Attr.type_ "number"
                    , onInput (String.toInt >> ChangeFilterValue)
                    ]
                    []
                ]
            , styled div
                [ property "display" "grid"
                , property "grid-gap" "16px"
                , property "grid" "1fr / auto-flow minmax(120px, max-content)"
                ]
                []
                [ viewButton
                    [ Attr.disabled <| isNothing model.diceCount
                    , Attr.type_ "submit"
                    ]
                    [ viewButtonText model.rolls ]
                , viewButton
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
            styled section
                [ padding2 (px 16) zero
                , position relative
                , before
                    [ backgroundColor transparent
                    , borderTop3 (px 2) solid Theme.theme.primary
                    , Theme.baseShadow
                    , property "content" "''"
                    , height (px 2)
                    , position absolute
                    , top zero
                    , width (pct 100)
                    ]
                ]
                []
                (if List.isEmpty rolls then
                    [ Html.em [] [ text "No More Dice!" ]
                    ]

                 else
                    [ viewStats rolls
                    , viewDice model rolls
                    ]
                )


viewDice : Model -> List Int -> Html Msg
viewDice { diceSize, selectedFilter, filterValue } rolls =
    let
        filterHelper value =
            Maybe.map (getFilterFn selectedFilter value) filterValue
                |> Maybe.withDefault True

        viewHelper value =
            Dice.view diceSize (filterHelper value) <| String.fromInt value
    in
    styled div
        [ displayFlex, flexWrap wrap ]
        []
        (List.map viewHelper rolls)


viewDiceSelector : Int -> Html Msg
viewDiceSelector selectedSize =
    let
        sizeSelector size =
            styled button
                [ borderWidth zero
                , Theme.baseFont
                , backgroundColor transparent
                ]
                [ Attr.type_ "button"
                , onClick <| ChangeSize size
                ]
                [ Dice.view size (selectedSize == size) <| String.fromInt size ]
    in
    div [ Attr.class "dice-selector" ] <| List.map sizeSelector diceSizes


viewStats : List Int -> Html Msg
viewStats rolls =
    let
        viewStat label value =
            text <| label ++ (String.fromInt <| Maybe.withDefault 0 <| value)
    in
    styled ul
        [ property "display" "grid"
        , property "grid" "1fr / auto-flow fit-content(100%)"
        , property "grid-gap" "24px"
        , listStyle none
        , marginTop zero
        , padding zero
        ]
        []
        [ li [] [ text <| "Average: " ++ (String.left 4 <| String.fromFloat <| average rolls) ]
        , li [] [ viewStat "Largest: " <| List.maximum rolls ]
        , li [] [ viewStat "Smallest: " <| List.minimum rolls ]
        ]


viewButton : List (Attribute msg) -> List (Html msg) -> Html msg
viewButton =
    styled button
        [ Theme.baseFont
        , backgroundColor transparent
        , border3 (px 2) solid Theme.theme.primary
        , Theme.baseShadow
        , cursor pointer
        , transition [ Css.Transitions.opacity3 200 0 Css.Transitions.ease ]
        , disabled
            [ cursor notAllowed
            , opacity (num 0.5)
            ]
        ]


viewInput : List (Attribute msg) -> List (Html msg) -> Html msg
viewInput =
    styled input
        [ Theme.baseFont
        , backgroundColor Theme.theme.primary
        , borderWidth zero
        , Theme.baseShadow
        , color Theme.theme.secondary
        , maxWidth (px 80)
        , padding (px 4)
        ]


viewLabel : List (Attribute msg) -> List (Html msg) -> Html msg
viewLabel =
    styled label
        [ display inlineBlock
        , margin4 zero (px 16) (px 16) zero
        ]


viewFormGroup : List (Html msg) -> Html msg
viewFormGroup =
    styled div
        [ marginBottom (px 16) ]
        []



-- UPDATE


type Msg
    = ApplyFilter
    | ChangeSize Int
    | ChangeFilterValue (Maybe Int)
    | ChangeDiceCount (Maybe Int)
    | FilterDropdownMsg (Select.Msg Filter)
    | Roll
    | RollResults (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ApplyFilter ->
            let
                filterHelper filterValue =
                    List.filter (getFilterFn model.selectedFilter filterValue)

                newRolls =
                    Maybe.map2 filterHelper model.filterValue model.rolls

                newDiceCount =
                    Maybe.map List.length newRolls
            in
            ( { model | rolls = newRolls, diceCount = newDiceCount }, Cmd.none )

        ChangeFilterValue value ->
            ( { model | filterValue = value }, Cmd.none )

        ChangeDiceCount count ->
            ( { model | diceCount = count, rolls = Nothing }, Cmd.none )

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
            ( { model | filterDropdown = newModel, selectedFilter = selected }
            , Cmd.none
            )

        Roll ->
            ( model, rollIfReady model )

        RollResults values ->
            ( { model | rolls = Just values }, Cmd.none )


rollIfReady : Model -> Cmd Msg
rollIfReady { diceSize, diceCount } =
    Maybe.map (rollDice diceSize) diceCount
        |> Maybe.withDefault Cmd.none


rollDice : Int -> Int -> Cmd Msg
rollDice size count =
    Random.int 1 size
        |> Random.list count
        |> Random.generate RollResults


getFilterFn : Filter -> (comparable -> comparable -> Bool)
getFilterFn filter =
    case filter of
        GreaterThan ->
            (>)

        GreaterThanEq ->
            (>=)

        LessThan ->
            (<)

        LessThanEq ->
            (<=)

        Equal ->
            (==)


filterBtnDisabled : Model -> Bool
filterBtnDisabled { filterValue, rolls, selectedFilter } =
    let
        filterFn value =
            not << getFilterFn selectedFilter value

        filtered value =
            List.isEmpty << List.filter (filterFn value)
    in
    Maybe.map2 filtered filterValue rolls
        |> Maybe.withDefault True
