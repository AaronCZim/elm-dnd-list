module DnDList.Drag exposing (..)

import Mouse


type alias Drag =
    { monitoring : Bool
    , down : Bool
    , drag : Bool
    , startI : Maybe Int
    , dragI : Maybe Int
    }


init =
    Drag False False False Nothing Nothing


type Msg
    = SetMonitoring Bool
    | EnterI Int
    | Leave
    | Move Mouse.Event
    | Down Mouse.Event
    | Up Mouse.Event


update : Msg -> Drag -> Drag
update msg model =
    case msg of
        SetMonitoring monitoring ->
            if monitoring then
                { model | monitoring = True }
            else
                init

        EnterI i ->
            if model.drag then
                { model | dragI = Just i }
            else
                { model | startI = Just i }

        Leave ->
            if model.drag then
                { model | dragI = Nothing }
            else
                { model | startI = Nothing }

        Move event ->
            if not model.down then
                model
            else
                { model | drag = True }

        Down event ->
            { model | down = True }

        Up event ->
            { init
                | monitoring = model.monitoring
                , startI =
                    case model.dragI of
                        Just _ ->
                            model.dragI

                        Nothing ->
                            model.startI
            }
