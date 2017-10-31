module DnDList.Drag exposing (..)

import Mouse


type Drag
    = NoDrag
    | DragTracking
    | Editing Int String
    | Hover Int
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
    | CommitHoveringOn Int
    | EnterI Int
    | Leave
    | Move Mouse.Event
    | Down Mouse.Event
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

        CommitHoveringOn i ->
            case model of
                Editing j val ->
                    Hover i

                _ ->
                    model

        EnterI i ->
            case model of
                Editing i val ->
                    model

                ActiveDrag { startI, dragI } ->
                    ActiveDrag { startI = startI, dragI = i }

                CancelDrag startI ->
                    ActiveDrag { startI = startI, dragI = i }

                _ ->
                    Hover i

        Leave ->
            case model of
                Hover i ->
                    DragTracking

                _ ->
                    model

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
