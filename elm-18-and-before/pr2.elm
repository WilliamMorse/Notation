-- skeleton program structure

import Html exposing (Html, button, div, text)
import Html.App as Html
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Model = Int -- define the model's type and structure

model : Model -- pass that in so we can initalize the state of the first model
model = -- start the counter at zero
  0

-- UPDATE
type Msg = Increment | Decrement | Reset -- two messages possible we will do something diferent for each

update : Msg -> Model -> Model -- no idea what this structure is....
update msg model =
  case msg of
    Increment -> -- if we get a n Increment message
      model + 1 -- add one to the model which is just a counter
    Decrement -> -- if we get a dec message
      model - 1 -- subtract from the model
    Reset ->
      0 -- set the model to the innitial value

-- VIEW

view : Model -> Html Msg
view model =
  div [] -- page divisons
    [ button [onClick Decrement ] [ text "Decrement"] -- define the message to send to the update function
    , div [] [ text (toString model) ] -- use the inbult toString function to change type from number to string
    , button [ onClick Increment ] [text "Increment"] -- onClick is the function that gets called when we click the button?
    , button [ onClick Reset ] [text "Reset"] -- add a reset button to the view
    ]
