module DnDList.Drag exposing (..)

import Mouse
import Keyboard
import Dom
import Task
import SelectClassName


type Drag
    = NoDrag
    | DragTracking
    | Editing Int String
    | DragStart Int
    | ActiveDrag { startI : Int, dragI : Int }
    | CancelDrag Int -- drag out of bounds undoes the drag


init =
    NoDrag


type Msg
    = NoOp
    | StartTracking
    | StopTracking
    | Edit Int String
    | Edition String
    | Commit
    | EnterI Int
    | Leave
    | Move Mouse.Event
    | Down Int Mouse.Event
    | Up Mouse.Event


update : Msg -> Drag -> ( Drag, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartTracking ->
            case model of
                NoDrag ->
                    ( DragTracking, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StopTracking ->
            case model of
                Editing i val ->
                    ( model, Cmd.none )

                _ ->
                    ( NoDrag, Cmd.none )

        Edit i value ->
            ( Editing i value, inputSelect i )

        Edition value ->
            case model of
                Editing i val ->
                    ( Editing i value, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Commit ->
            case model of
                Editing i val ->
                    ( DragTracking, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EnterI i ->
            case model of
                Editing j value ->
                    ( model, Cmd.none )

                ActiveDrag { startI, dragI } ->
                    ( ActiveDrag { startI = startI, dragI = i }, Cmd.none )

                CancelDrag startI ->
                    ( ActiveDrag { startI = startI, dragI = i }, Cmd.none )

                _ ->
                    ( DragTracking, Cmd.none )

        Leave ->
            case model of
                Editing i value ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Move event ->
            case model of
                DragStart i ->
                    ( ActiveDrag { startI = i, dragI = i }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Down i event ->
            case model of
                DragTracking ->
                    ( DragStart i, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Up event ->
            case model of
                ActiveDrag { startI, dragI } ->
                    ( DragTracking, Cmd.none )

                DragStart i ->
                    ( DragTracking, Cmd.none )

                _ ->
                    ( model, Cmd.none )


editingInputClass =
    "dnd_list_editing_input"


inputSelect i =
    ("." ++ editingInputClass)
        |> SelectClassName.select
        |> Task.attempt (\_ -> NoOp)
