module Select exposing (Item, Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Json.Decode as Decode



-- MODEL


type alias Item a =
    ( String, a )


type alias Model a =
    { open : Bool
    , items : List (Item a)
    , selected : Item a
    }


init : List (Item a) -> Item a -> Model a
init =
    Model False



-- VIEW


view : Model a -> Html (Msg a)
view model =
    div [ Attr.class "select" ] <|
        [ div [ Attr.class "select-selected", onClick Toggle ]
            [ text <| Tuple.first model.selected
            ]
        ]
            ++ viewList model


viewList : Model a -> List (Html (Msg a))
viewList model =
    if model.open then
        [ ul [ Attr.class "select-list" ] <|
            List.map viewItem model.items
        ]

    else
        []


viewItem : Item a -> Html (Msg a)
viewItem item =
    li [ onClick <| SelectItem item ]
        [ text <| Tuple.first item ]



-- UPDATE


type Msg a
    = Click
    | Toggle
    | SelectItem (Item a)


update : Msg a -> Model a -> ( Model a, a )
update msg model =
    case msg of
        Click ->
            ( { model | open = False }
            , Tuple.second model.selected
            )

        SelectItem item ->
            ( { model | open = False, selected = item }
            , Tuple.second item
            )

        Toggle ->
            ( { model | open = not model.open }
            , Tuple.second model.selected
            )



-- SUBS


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    if model.open then
        Browser.Events.onClick (Decode.succeed Click)

    else
        Sub.none
