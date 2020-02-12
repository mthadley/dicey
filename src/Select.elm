module Select exposing (Item, Model, Msg, init, update, view)

import Browser.Events
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import List.Extra
import Theme



-- MODEL


type alias Item a =
    ( String, a )


type alias Model a =
    { items : List (Item a)
    , selected : Item a
    }


init : List (Item a) -> Item a -> Model a
init =
    Model



-- VIEW


view : Model a -> Html (Msg a)
view model =
    styled div
        [ Theme.baseShadow
        , cursor pointer
        , display inlineBlock
        , position relative
        , backgroundColor Theme.theme.primary
        , color Theme.theme.secondary
        , padding4 (px 8) (px 32) (px 8) (px 8)
        , let
            arrowSize =
                px 8
          in
          after
            [ borderLeft3 arrowSize solid transparent
            , borderRight3 arrowSize solid transparent
            , borderTop3 arrowSize solid Theme.theme.secondary
            , property "content" "''"
            , height zero
            , pointerEvents none
            , position absolute
            , right (px 5)
            , top (px 16)
            , width zero
            ]
        ]
        []
        [ text <| Tuple.first model.selected
        , viewList model
        ]


viewList : Model a -> Html (Msg a)
viewList model =
    select
        [ Attr.class "select-list"
        , Attr.style "position" "absolute"
        , Attr.style "top" "0"
        , Attr.style "bottom" "0"
        , Attr.style "left" "0"
        , Attr.style "right" "0"
        , Attr.style "width" "100%"
        , Attr.style "opacity" "0"
        , Attr.style "-webkit-appearance" "none"
        , Attr.style "-moz-appearance" "none"
        , Attr.style "appearance" "none"
        , on "change" (Decode.map SelectItem targetValue)
        ]
        (List.map viewItem model.items)


viewItem : Item a -> Html (Msg a)
viewItem ( label, _ ) =
    option [ Attr.value label ]
        [ text label ]



-- UPDATE


type Msg a
    = SelectItem String


update : Msg a -> Model a -> ( Model a, a )
update msg model =
    case msg of
        SelectItem value ->
            let
                item =
                    List.Extra.find (\( label, _ ) -> label == value) model.items
                        |> Maybe.withDefault model.selected
            in
            ( { model | selected = item }
            , Tuple.second item
            )
