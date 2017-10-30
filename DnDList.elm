module DnDList exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Mouse
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import DnDList.Drag as Drag


init =
    fromList []


init3 =
    fromList [ "one", "two", "three" ]


fromList list =
    Model list "" Drag.init


type alias Model =
    { list : List String
    , input : String
    , drag : Drag.Drag
    }


type Msg
    = Append String
    | Remove Int
    | TextInput String
    | DragMsg Drag.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Append value ->
            { model
                | list = model.list ++ [ value ]
                , input = ""
            }

        Remove i ->
            { model
                | list =
                    (model.list
                        |> List.take i
                    )
                        ++ (model.list
                                |> List.drop (i + 1)
                           )
            }

        TextInput str ->
            { model | input = str }

        DragMsg msg ->
            case msg of
                Drag.Up event ->
                    dragUp event model

                _ ->
                    { model | drag = Drag.update msg model.drag }


dragUp : Mouse.Event -> Model -> Model
dragUp event model =
    let
        newDrag =
            Drag.update (Drag.Up event) model.drag
    in
        case model.drag of
            Drag.ActiveDrag { startI, dragI } ->
                case newDrag of
                    Drag.Hover i ->
                        { model
                            | drag = newDrag
                            , list =
                                model.list
                                    |> swap startI dragI
                                    |> Maybe.withDefault model.list
                        }

                    _ ->
                        { model | drag = newDrag }

            _ ->
                { model | drag = newDrag }


swap : Int -> Int -> List String -> Maybe (List String)
swap i j list =
    let
        a =
            Basics.min i j

        b =
            Basics.max i j

        len =
            list |> List.length
    in
        let
            av =
                list
                    |> List.drop a
                    |> List.head
                    |> Maybe.withDefault ""

            bv =
                list
                    |> List.drop b
                    |> List.head
                    |> Maybe.withDefault ""
        in
            if a < 0 || b < 0 || a >= len || b >= len then
                Nothing
            else if a == b then
                Just list
            else
                Just
                    ((list |> List.take a)
                        ++ [ bv ]
                        ++ (list |> List.take b |> List.drop (a + 1))
                        ++ [ av ]
                        ++ (list |> List.drop (b + 1))
                    )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [ Col.xs12 ]
                [ Card.config []
                    |> Card.header []
                        [ h2 [] [ text "Drag and Drop List" ] ]
                    |> Card.block []
                        [ Card.custom <| appendItemView model
                        , Card.custom <| hr [] []
                        , listView model
                        ]
                    |> Card.view
                ]
            , Grid.col [ Col.xs12 ] [ model |> toString |> text ]
            ]
        ]


appendItemView : Model -> Html Msg
appendItemView model =
    InputGroup.config
        (InputGroup.text
            [ Input.placeholder "label"
            , Input.value model.input
            , Input.onInput TextInput
            ]
        )
        |> InputGroup.predecessors [ InputGroup.span [] [ text "New Item" ] ]
        |> InputGroup.successors
            [ InputGroup.button
                [ Button.secondary
                , Append model.input |> Button.onClick
                ]
                [ text "Add" ]
            ]
        |> InputGroup.view


listView : Model -> Card.BlockItem Msg
listView model =
    Card.custom <|
        (Html.map mapDragMsg
            (div
                [ Drag.Move |> Mouse.onMove
                , Drag.Down |> Mouse.onDown
                , Drag.Up |> Mouse.onUp
                ]
                [ ListGroup.ul
                    (model.list
                        |> List.indexedMap (listItemView model)
                    )
                ]
            )
        )


mapDragMsg : Drag.Msg -> Msg
mapDragMsg msg =
    DragMsg msg


listItemView : Model -> Int -> String -> ListGroup.Item Drag.Msg
listItemView model i item =
    let
        active =
            case model.drag of
                Drag.ActiveDrag { startI, dragI } ->
                    i == startI || i == dragI

                _ ->
                    False
    in
        ListGroup.li
            ([ ListGroup.attrs
                [ (Drag.EnterI i) |> onMouseEnter
                , Drag.Leave |> onMouseLeave
                ]
             ]
                ++ if active then
                    [ ListGroup.active ]
                   else
                    []
            )
            [ text item ]
