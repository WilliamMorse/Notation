-- list testing

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main =
  Html.beginnerProgram { model = model, view = view, update = update }

-- model

type alias Model =
  { testList : List String
  }

model : Model
model =
  Model []

type Msg
    = UpdateList String

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateList newEntry ->
      { model | testList = model.testList ++ [newEntry] }

-- view
view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "this is the placeholder", onInput UpdateList ] []
    , ul : List ()
    ]
