module DnDList exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dom
import Task
import Mouse
import Keyboard
import Char
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import DnDList.Drag as Drag
import SelectDom


textInputClass =
    "dnd_list_add_item_label_input"


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyPress


inputFocus =
    ("." ++ textInputClass)
        |> SelectDom.focus
        |> Task.attempt (\_ -> NoOp)


init =
    ( initModel, Cmd.none )


initModel =
    modelFromList []


init3 =
    ( initModel, Cmd.none )


initModel3 =
    modelFromList [ "one", "two", "three" ]


fromList =
    ( modelFromList, Cmd.none )


modelFromList list =
    Model list "" Drag.init


type alias Model =
    { list : List String
    , input : String
    , drag : Drag.Drag
    }


type Msg
    = NoOp
    | Append String
    | Remove Int
    | TextInput String
    | KeyPress Keyboard.KeyCode
    | DragMsg Drag.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Append value ->
            ( { model
                | list = model.list ++ [ value ]
                , input = ""
              }
            , inputFocus
            )

        Remove i ->
            ( { model | list = model.list |> remove i }
            , Cmd.none
            )

        TextInput str ->
            ( { model | input = str }
            , Cmd.none
            )

        KeyPress keyCode ->
            -- KeyCode 13 is the return key
            if keyCode == 13 then
                case model.drag of
                    Drag.Editing i value ->
                        let
                            ( dragModel, dragCmd ) =
                                Drag.update Drag.Commit model.drag
                        in
                            ( { model
                                | drag = dragModel
                                , list = commitList model
                              }
                            , dragCmd
                                |> Cmd.map (\msg -> DragMsg msg)
                            )

                    _ ->
                        ( model, Cmd.none )
            else
                ( model, Cmd.none )

        DragMsg msg ->
            let
                ( dragModel, dragCmd ) =
                    Drag.update msg model.drag
            in
                case msg of
                    Drag.Up event ->
                        dragUp event model dragModel dragCmd

                    Drag.Commit ->
                        ( { model
                            | drag = dragModel
                            , list = commitList model
                          }
                        , dragCmd
                            |> Cmd.map (\msg -> DragMsg msg)
                        )

                    _ ->
                        ( { model | drag = dragModel }
                        , dragCmd
                            |> Cmd.map (\msg -> DragMsg msg)
                        )


commitList model =
    case model.drag of
        Drag.Editing i value ->
            if value == "" then
                model.list |> remove i
            else
                model.list |> set i value

        _ ->
            model.list


set : Int -> String -> List String -> List String
set i value list =
    (list
        |> List.take i
    )
        ++ (value :: (list |> List.drop (i + 1)))


dragUp : Mouse.Event -> Model -> Drag.Drag -> Cmd Drag.Msg -> ( Model, Cmd Msg )
dragUp event model dragModel dragCmd =
    case model.drag of
        Drag.ActiveDrag { startI, dragI } ->
            ( { model
                | drag = dragModel
                , list =
                    model.list
                        |> swap startI dragI
                        |> Maybe.withDefault model.list
              }
            , dragCmd
                |> Cmd.map (\msg -> DragMsg msg)
            )

        _ ->
            ( { model | drag = dragModel }
            , dragCmd
                |> Cmd.map (\msg -> DragMsg msg)
            )


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


remove : Int -> List String -> List String
remove i list =
    (list
        |> List.take i
    )
        ++ (list
                |> List.drop (i + 1)
           )


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , Grid.row []
            [ Grid.col [ Col.xs12 ]
                [ Card.config
                    [ Card.attrs
                        [ DragMsg Drag.StartTracking |> onMouseEnter
                        , DragMsg Drag.StopTracking |> onMouseLeave
                        ]
                    ]
                    |> Card.header []
                        [ h2 [] [ text "Drop and Drop List" ] ]
                    |> Card.block []
                        [ Card.custom <| appendItemView model ]
                    |> Card.block []
                        [ listView model
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
            , Input.attrs [ class textInputClass ]
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
        (Html.map (\msg -> DragMsg msg)
            (div
                (case model.drag of
                    Drag.Editing i value ->
                        []

                    _ ->
                        [ Drag.Move |> Mouse.onMove
                        , Drag.Up |> Mouse.onUp
                        ]
                )
                [ ListGroup.ul
                    (model.list
                        |> List.indexedMap (listItemView model)
                    )
                ]
            )
        )


listItemView : Model -> Int -> String -> ListGroup.Item Drag.Msg
listItemView model i item =
    case model.drag of
        Drag.Editing j value ->
            if j == i then
                editingListItemView i value
            else
                displayListItemView False i item

        Drag.ActiveDrag { startI, dragI } ->
            displayListItemView
                (i == startI || i == dragI)
                i
                item

        _ ->
            displayListItemView False i item


displayListItemView : Bool -> Int -> String -> ListGroup.Item Drag.Msg
displayListItemView active i item =
    ListGroup.li
        ([ ListGroup.attrs
            [ (Drag.EnterI i) |> onMouseEnter
            , Drag.Leave |> onMouseLeave
            , (Drag.Down i) |> Mouse.onDown
            , Drag.Commit |> onClick
            , (Drag.Edit i item) |> onDoubleClick
            ]
         ]
            ++ if active then
                [ ListGroup.active ]
               else
                []
        )
        [ text item ]


editingListItemView : Int -> String -> ListGroup.Item Drag.Msg
editingListItemView i item =
    ListGroup.li
        ([ ListGroup.attrs
            [ (Drag.EnterI i) |> onMouseEnter
            , Drag.Leave |> onMouseLeave
            ]
         , ListGroup.active
         ]
        )
        [ Input.text
            [ Input.value item
            , Input.attrs
                [ (Drag.Edition) |> onInput
                , class Drag.editingInputClass
                ]
            ]
        ]
