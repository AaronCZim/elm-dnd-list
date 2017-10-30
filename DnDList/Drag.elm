module DnDList.Drag exposing (..)

import Mouse


type Drag
    = NoDrag
    | Hover Int
    | DragStart Int
    | ActiveDrag { startI : Int, dragI : Int }
    | CancelDrag Int -- drag out of bounds undoes the drag


init =
    NoDrag


type Msg
    = EnterI Int
    | Leave
    | Move Mouse.Event
    | Down Mouse.Event
    | Up Mouse.Event


update : Msg -> Drag -> Drag
update msg model =
    case msg of
        EnterI i ->
            case model of
                ActiveDrag { startI, dragI } ->
                    ActiveDrag { startI = startI, dragI = i }

                CancelDrag startI ->
                    ActiveDrag { startI = startI, dragI = i }

                _ ->
                    Hover i

        Leave ->
            case model of
                ActiveDrag { startI, dragI } ->
                    CancelDrag startI

                _ ->
                    NoDrag

        Move event ->
            case model of
                DragStart i ->
                    ActiveDrag { startI = i, dragI = i }

                _ ->
                    model

        Down event ->
            case model of
                Hover i ->
                    DragStart i

                _ ->
                    model

        Up event ->
            case model of
                ActiveDrag { startI, dragI } ->
                    Hover dragI

                DragStart i ->
                    Hover i

                _ ->
                    model
