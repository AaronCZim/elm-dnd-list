module Main exposing (..)

import Html exposing (..)
import DnDList exposing (..)


main =
    Html.program
        { init = ( init3, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
