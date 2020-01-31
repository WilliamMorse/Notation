module Main exposing (Msg(..), main, model, update, view)

import Html exposing (Html, button, div, text)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)



--import StartApp.Simple as StartApp


main =
    beginnerProgram { model = model, view = view, update = update }


model =
    0


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


type Msg
    = Increment
    | Decrement


update action model =
    case action of
        Increment ->
            model + 1

        Decrement ->
            model - 1
