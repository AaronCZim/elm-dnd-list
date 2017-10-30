module Main exposing (..)

import Html exposing (..)
import DnDList exposing (..)


main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
