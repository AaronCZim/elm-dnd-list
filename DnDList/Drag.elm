module DnDList.Drag exposing (..)

import Mouse


type alias Drag =
    { down : Bool -- is mouse down
    , drag : Bool -- is mouse down and moved from click location
    , startI : Maybe Int
    , dragI : Maybe Int
    }


init =
    Drag False False Nothing Nothing


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
                | startI =
                    case model.dragI of
                        Just _ ->
                            model.dragI

                        Nothing ->
                            -- if the click
                            model.startI
            }
