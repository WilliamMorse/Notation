import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import List as List exposing (..)


--main =
--  Html.beginnerProgram { model = model, view = view, update = update }
main : Program Never
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- model
type alias Model =
  {
  , laTeX : String -- edit to the current nodes laTeX string
  , equation_list : List String -- List Step
  , current_equation : Int
  }

--model : Model
init : ( Model, Cmd Msg )
init =
    ( [] 100
    , Cmd.None )


-- update
type Msg
    = LaTeX String
    | Edit Int
    | RenderEquation

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LaTeX s ->
      { model | laTeX = s } ! []
