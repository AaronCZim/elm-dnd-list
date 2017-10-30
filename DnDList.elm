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


fromList list =
    Model list "" Nothing Drag.init


type alias Model =
    { list : List String
    , input : String
    , caret : Maybe Int
    , drag : Drag.Drag
    }


type Msg
    = Append String
    | Remove Int
    | TextInput String
    | DragMsg Drag.Msg
    | DragMove Mouse.Event
    | DragDown Mouse.Event
    | DragUp Mouse.Event


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
            { model | drag = Drag.update msg model.drag }

        DragMove event ->
            { model | drag = Drag.update (Drag.Move event) model.drag }

        DragDown event ->
            { model | drag = Drag.update (Drag.Down event) model.drag }

        DragUp event ->
            { model | drag = Drag.update (Drag.Up event) model.drag }


dragUp : Mouse.Event -> Model -> Model
dragUp event model =
    let
        newDrag =
            Drag.update (Drag.Up event) model.drag
    in
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
                [ Card.config
                    [ Card.attrs
                        ([ DragMsg (Drag.SetMonitoring True) |> onMouseEnter
                         , DragMsg (Drag.SetMonitoring False) |> onMouseLeave
                         ]
                        )
                    ]
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


listView model =
    Card.custom <|
        div []
            [ ListGroup.ul
                (model.list
                    |> List.indexedMap (listItemView model)
                )
            ]


listItemView model i item =
    ListGroup.li
        ([ ListGroup.attrs
            (if model.drag.monitoring then
                [ DragMsg (Drag.EnterI i) |> onMouseEnter
                , DragMsg (Drag.Leave) |> onMouseLeave
                ]
             else
                []
            )
         ]
        )
        [ text item ]
