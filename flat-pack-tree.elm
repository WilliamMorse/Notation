module Main exposing (Model)

import Array exposing (..)



-- here we can flatpack the array


type alias Model =
    { ids : Array Int
    , depth : Array Int
    , operations : Array String
    , equations : Array String
    , notes : Array String
    }
