module Select exposing (Item, Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Tuple exposing (first, second)


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
            [ text <| first model.selected
            ]
        ]
            ++ (viewList model)


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
        [ text <| first item ]



-- UPDATE


type Msg a
    = Toggle
    | SelectItem (Item a)


update : Msg a -> Model a -> ( Model a, a )
update msg model =
    case msg of
        SelectItem item ->
            ( { model | open = False, selected = item }, second item )

        Toggle ->
            ( { model | open = not model.open }, second model.selected )