module Main exposing (Model, Msg(..), main, model, update, view)

import Html exposing (Attribute, Html, div, input, text)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String


main =
    Html.beginnerProgram { model = model, view = view, update = update }



--model


type alias Model =
    { content : String
    , reversedContent : String
    }


model : Model
model =
    { content = ""
    , reversedContent = "" -- also store a reversed string
    }



-- just keep a string that holds the text
-- update


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent, reversedContent = String.reverse newContent }



-- view


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "text to reverse", onInput Change ] []
        , div [] [ text model.reversedContent ]
        ]



-- why are they applying operations in the view ???
