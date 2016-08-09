import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

-- Model

type alias Model =
  { operation : String
  , equation : String
  , comments : String
  }

model : Model
model =
  Model "" "" ""



-- Update

type Msg
    = Operation String
    | Equation String
    | Comments String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Operation operation ->
      {model | operation = operation }

    Equation equation ->
      { model | equation = equation }

    Comments equation ->
      { model | comments = equation }

-- view

view : Model -> Html Msg
view model =
  div []
    [ input [ type' "text", placeholder "Operation", onInput Operation ] []
    , input [ type' "equation", placeholder "Equation", onInput Equation ] []
    , input [ type' "equation", placeholder "Comments", onInput Comments ] []
    , viewValidation model
    ]

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.equation == model.comments then
        ("green", "OK")
      else
        ("blue", "unknown operation")
  in
    div [ style [("color", color)] ] [ text message ]
