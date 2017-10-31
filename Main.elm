module Main exposing (..)

import Html exposing (..)
import DnDList exposing (..)


main =
    Html.program
        { init =
            ( initModel3
            , inputFocus
            )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
