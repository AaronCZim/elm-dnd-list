module Main exposing (..)

import Html exposing (..)
import DnDList exposing (..)


main =
    Html.beginnerProgram
        { model = init3
        , update = update
        , view = view
        }
