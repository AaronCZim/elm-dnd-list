module DnDList.Drag exposing (..)

import Mouse
import Keyboard


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
    = StartTracking
    | StopTracking
    | Edit Int String
    | Edition String
    | Commit
    | EnterI Int
    | Leave
    | Move Mouse.Event
    | Down Int Mouse.Event
    | Up Mouse.Event


update : Msg -> Drag -> Drag
update msg model =
    case msg of
        StartTracking ->
            case model of
                NoDrag ->
                    DragTracking

                _ ->
                    model

        StopTracking ->
            case model of
                Editing i val ->
                    model

                _ ->
                    NoDrag

        Edit i value ->
            Editing i value

        Edition value ->
            case model of
                Editing i val ->
                    Editing i value

                _ ->
                    model

        Commit ->
            case model of
                Editing i val ->
                    DragTracking

                _ ->
                    model

        EnterI i ->
            case model of
                Editing j value ->
                    model

                ActiveDrag { startI, dragI } ->
                    ActiveDrag { startI = startI, dragI = i }

                CancelDrag startI ->
                    ActiveDrag { startI = startI, dragI = i }

                _ ->
                    DragTracking

        Leave ->
            case model of
                Editing i value ->
                    model

                _ ->
                    model

        Move event ->
            case model of
                DragStart i ->
                    ActiveDrag { startI = i, dragI = i }

                _ ->
                    model

        Down i event ->
            case model of
                DragTracking ->
                    DragStart i

                _ ->
                    model

        Up event ->
            case model of
                ActiveDrag { startI, dragI } ->
                    DragTracking

                DragStart i ->
                    DragTracking

                _ ->
                    model
